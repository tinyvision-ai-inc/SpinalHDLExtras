# Bus-error logger

## Problem

On SpineX, a load or store that hits no slave, a slave that never responds, or a
register offset that is not implemented can stall the
PipelinedMemoryBus (PMB) forever. `devmem` and the UART console freeze. If the
cycle is completed with data `0`, that is indistinguishable from a valid idle or
power-on value.

The debug trace RAM (EventLogger at `0xe0006000`) is already in use. Putting
interconnect faults into that same FIFO would drop traces when the bus is
misbehaving — which is when you need both.

This Spinal PMB has no `rsp.error`. Completing a bad access as a RISC-V load/store
access fault (`mcause` 7) would mean changing Spinal or Vex. That is out of scope.

## Solution

**Always finish the cycle.** A miss, timeout, or unimplemented offset returns a
fixed sentinel (never `0`) so software can tell failure modes apart. Stores
complete; they do not trap.

**Optionally record the event.** A second FlowLogger — same block and CSR layout
as EventLogger, separate RAM — sits at `0xe0008000` (`BusErrorLogger`). Interrupt
**5** on `litex,vexriscv-intc0` is asserted while that FIFO’s occupancy is not
zero, and clears when firmware reads the records out.

EventLogger stays at `0xe0006000` on `GlobalLogger`. Bus errors must not reuse
that instance.

The miss, timeout, and unimplemented-offset paths sit on the interconnect
(dBus, iBus, APB decoder, timeout wrapper), not inside UART, I2C, or the timer.
An unused offset in a peripheral’s register file still completes locally and
reports on that bus’s unimplemented channel (`dbus_unimpl`), not a per-block
channel.

YAML `withBusErrorLogger` (default true), `busErrorLogDepth` (default 32), and
`busErrorLocalDepth` (default 4, per-channel `Flow.toStream` queue) turn the
RAM and IRQ off. Completing with a sentinel still happens. Plugin
`BusErrorPlugin` is first in `SpinexConfig.plugins` so logging is enabled before
register files elaborate (`BusError.loggingEnabled`). Camera boards
(`dual_imx219.yml` `spinexConfig`) call `busErrorPluginFirst` after USB/WB
plugins are prepended so that still holds.

## What gets logged

The log stores a 3-bit **cause**. AXI `RESP` is only two bits, so UNIMPL /
TIMEOUT / PROTO share SLVERR or DECERR on AXI and keep the extra detail in the
log.

| Cause | Value | Read data | AXI RESP |
|---|---|---|---|
| DECERR | 1 | `0xDEADBEEF` | DECERR |
| SLVERR | 2 | `0x5A5A5A5A` | SLVERR |
| UNIMPL | 3 | `0xA5A5A5A5` | SLVERR |
| TIMEOUT | 4 | RSP `0xDEADBEEF`, CMD `0xCAFED00D` | SLVERR |
| PROTO | 5 | `0xDEADBEEF` | DECERR |

Firmware sees the sentinel on the load, the FIFO record, or both.

Several sources can fire in one cycle. The bus-error FlowLogger uses `stageAllFlows` when `busErrorLocalDepth` is
nonzero, so each channel has a small LUT-RAM skid (`queue(localDepth)`).
`localDepth = 0` / `stageAllFlows = false` was the congestion setting: same-cycle
dual events can drop (occupancy/dropped still count). EventLogger may still use
`localDepth` from YAML. A Wishbone miss storm can still overflow the 32-deep RAM
and the firmware msgq; this skid is for sparse / colliding pulses, not a
continuous 75 MHz firehose.

Unimplemented offsets on one bus share one channel: each hole calls
`BusError.unimplTap`; the plugin merges them (`queue(1)` then lower-first) to
`dbus_unimpl` (or `ibus_unimpl` when those buses exist). **Only holes elaborated
inside SpineX are merged.** TinyClunx / Wishbone BusIf holes still complete
locally with `0xA5A5A5A5`; they are not wired into the SpineX FIFO (sibling
hierarchy).

Typical SpinexMinimal / debug SoC channels:

| Channel | Source |
|---|---|
| `pmb_decode_miss_d` | dBus: no slave decoded the address |
| `pmb_decode_miss_i` | iBus: no slave decoded the address |
| `dbus_timeout` | PMB timeout wrapper |
| `dbus_unimpl` | SpineX dBus unimplemented offsets |
| `apb_decode_miss` | APB decoder: no hit (full SoC) |

`BusErrorTestPlugin` is **only** on `SpinexConfig.minimal` (SpinexMinimal). It
maps UNIMPL / TIMEOUT / SLVERR / PROTO at `0xe0008500`–`0xe00088ff`. Production
camera (`dual_imx219`) does **not** instantiate that plugin. Those addresses are
unmapped PMB there.

FlowLogger also writes metadata records (time / signature). Software should skip
index-all-1s words.

## CSR map

Same map as EventLogger. Base `0xe0008000`. Little-endian
`readStreamNonBlocking` of the 95-bit log word: bit 0 of `+8` is valid; payload
follows; the pop happens on the last word (`+16`).

DT `reg-names` on `bus_error@e0008000` (`spinex,bus-error`). Same map as EventLogger.

| Offset | `reg-names` | Field |
|---|---|---|
| `+0` | `ctrl` | ctrl |
| `+4` | `captured_events` | captured |
| `+8`..`+16` | `stream` | valid plus 95-bit record (pop on last word) |
| `+20` | `checksum` | checksum |
| `+24` | `sysclk_lsb` | sysclk LSB |
| `+28` | `fifo_occupancy` | occupancy; write flushes the FIFO and zeros captured, dropped, and per-channel counts |
| `+32` | `channel_count` | number of FlowLogger taps (`datas.size`); write is manual trigger |
| `+36` | `inactive_mask` | bit `i` quiet (no log / no `flowCnt`) for that tap; bus still completes |
| `+48` | `signature` | signature |
| `+52` | `dropped_events` | dropped (write clears) |
| `+56` | `event_counters` | per-channel fire counts |

`channel_count` is **how many taps were elaborated**, not how many have fired.
It is a compile-time constant. `bus_error clear` zeros occupancy, dropped,
`msgq_drop`, and the per-channel counts; it does not change `channel_count` or
`inactive_mask`.
Zeros on every named row means none of those taps have counted since the last
clear.

Shell `bus_error info` prints `[index] name count` from DT `channel-names` (same
order as FlowLogger / `event_counters`). A leading `*` means that channel is
masked. `bus_error mask` / `unmask` write `inactive_mask` (same CSR as EventLogger
`events mask`). Arguments are indices or those names; no args mask or unmask
all. Completers still finish the bus. Spinal overlay emits `channel-names` from
the logger taps. The SoC dtsi must match until the overlay is the firmware DT.

Camera (`dual_imx219`) is always **6** taps:

| Index | Channel | Source |
|---|---|---|
| 0 | `pmb_decode_miss_d` | SpineX dBus miss completer |
| 1 | `pmb_decode_miss_i` | iBus miss completer |
| 2 | `axi_usb_bus_error` | USB AXI ingest |
| 3 | `axi_mmi_bus_error` | MMI AXI ingest |
| 4 | `wb_decode_miss` | Wishbone ingest |
| 5 | `apb_decode_miss` | APB decoder miss in SpineX |

SpinexMinimal uses a different N (test slaves / `dbus_unimpl`; no USB/MMI/WB ingest).

## Stimulus addresses (which SoC)

The logger CSR window is **1 KiB**: `0xe0008000`–`0xe00083ff`. Anything at
`0xe0008400` and above is not a logger register.

### SpinexMinimal only (`BusErrorTestPlugin`)

Use `samples/bus_error` on this image. SRAM HIL, not the camera bitfile.

| Address | Expected load | Log |
|---|---|---|
| unmapped (e.g. `0x50000000`) | `0xDEADBEEF` | `pmb_decode_miss_d` DECERR |
| `0xe0008504` (hole in `buserr_unimpl` 256 B at `0xe0008500`) | `0xA5A5A5A5` | `dbus_unimpl` UNIMPL |
| `0xe0008600` (`buserr_timeout`) | `0xCAFED00D` | `dbus_timeout` TIMEOUT (CMD; rsp held the cycle after `cmd.ready`) |
| `0xe0008700` (`buserr_slverr`) | `0x5A5A5A5A` | `pmb_slverr` SLVERR |
| `0xe0008800` (`buserr_proto`) | `0xDEADBEEF` | `pmb_proto` PROTO |

### Camera (`dual_imx219`) — HIL 2026-09-08

No `BusErrorTestPlugin`. Channel order: `[0]` `pmb_decode_miss_d`, `[1]` `_i`,
`[2]` `axi_usb_bus_error`, `[3]` `axi_mmi_bus_error`, `[4]` `wb_decode_miss`,
`[5]` `apb_decode_miss`. There is no timeout / SLVERR / PROTO / `dbus_unimpl`
window.

| Address | Load | What actually happens |
|---|---|---|
| `0x50000000` | `0xDEADBEEF` | PMB decode miss; `[0]` increments |
| `0xb1020000` (past 128 KiB LRAM) | `0xDEADBEEF` | MMI AXI DECERR; `[3]` increments |
| `0xe0008504`, `0xe0008600`, `0xe0008700`, `0xe0008800` | `0xDEADBEEF` | **Also PMB decode miss** (`[0]`). Not UNIMPL / TIMEOUT / SLVERR / PROTO. Past the 1 KiB logger map, no test slaves. |

`[4]` can already be huge (`wb_decode_miss` storm) before any of these loads.
Do not treat that count as a scoreboard for the `0xe00085xx` pokes.

UNIMPL `0xA5A5A5A5` still needs a **mapped** SpineX BusIf hole (reserved offset
inside a slave window). The camera HIL has not identified a stable `devmem`
address for that path yet. TinyClunx / WB BusIf holes may complete with
`0xA5A5A5A5` without incrementing this logger.

## Completers

| Path | Finish | Read data | Log |
|---|---|---|---|
| PMB decode miss (`PmbMissCompleter`) | always | `0xDEADBEEF` | `pmb_decode_miss_{d,i}` |
| PMB / WB BusIf reserved offset | always | `0xA5A5A5A5` | `dbus_unimpl` if the BusIf is in SpineX |
| AXI no slave (`AxiMissCompleter`) | burst, tiled lanes | `0xDEADBEEF` per 32-bit lane | `axi_usb_bus_error` / `axi_mmi_bus_error` |
| AXI→PMB (`sentinelResp`, default on) | RRESP from PMB data | same sentinels | same AXI taps if RESP ≠ OKAY |
| WB interconnect miss | ACK + ERR | `0xDEADBEEF` | `wb_decode_miss` (ingest) |

Mapped, implemented RAM/CSR beats stay OKAY / ACK. A hole in a mapped window is
UNIMPL, not DECERR. Unmapped AXI in the 24-bit fabric is DECERR (no slave), not
UNIMPL.

Do not set Vex `dBus.rsp.error` (no `mcause` 7).

## Camera SoC (`dual_imx219`)

The logger RAM is inside SpineX. TinyClunx and the Wishbone fabric are
siblings, so AXI/WB events are **slave Flow ports** on SpineX
(`axi_usb_bus_error`, `axi_mmi_bus_error`, `wb_decode_miss`). TinyvisionTopLevel
drives them. USB AXI is CDC’d from the 100 MHz USB clock into the CPU domain
before that port.

Generated FlowLogger channels: the six rows under CSR `channel_count` above.
USB/MMI AXI taps fire on non-OKAY B, or R last.

AXI: USB TRB LRAM is `[0, usbBufferSize)` on the 24-bit bus (default 128 KiB).
USB HIP and CPU MMI share a **2×1 AXI arbiter**, then `AxiRamOrMiss` (1×2)
hits LRAM or `AxiMissCompleter`. That avoids a 2×2 64-bit crossbar (PAR
congestion). USB AXI bus-error Flows cross to the CPU clock with
`toStream` + `queue(axiCdcDepth)` (same depth as the other AXI CDCs; overflow
if the push side fills). `FlowCCByToggle` was one-deep and dropped a second
error before handshake.

Firmware check from the CPU (not the SpinexMinimal scoreboard). Use the camera
table above. Do not `devmem 0xe0008504` / `8600` / `8700` / `8800` expecting
`0xA5A5A5A5` / `0xCAFED00D` / `0x5A5A5A5A`.

DT `sram1@b1000000` size `0x2000` is the **CPU-reserved TRB pages**, not the AXI
decode size. Do not use that 8 KiB as the miss boundary.

`samples/bus_error` is SpinexMinimal only.

## Area

Radiant Synplify 2025.2.1, LIFCL mapper, isolated `BusErrorAreaDut` (four
channels, depth 32) versus completers only:

| | LUT4 | regs | DP16K |
|---|---|---|---|
| Logger on | 482 | 1714 | 6 |
| Completers only | 13 | 18 | 0 |

The 95-bit × 32 true-dual-port FIFO maps to six `DP16K`, not one EBR.
`synth_nexus` is not a substitute (Lattice RAM IP is a blackbox).

One FlowLogger RAM for the SoC. Extra buses add FlowLogger inputs on that RAM,
not another EventLogger-sized block.

## Firmware

Zephyr compatible `spinex,bus-error` at `0xe0008000` (SoC DT
`tinyclunx33_rtl.dtsi`; dual_imx219 picks this up via `rtl_1_2`). That is not
`ctrl0` `reg-names = "bus_errors"` at `0xe0000008`.

| File | Role |
|---|---|
| `zephyr/dts/bindings/debug/spinex,bus-error.yaml` | binding |
| `zephyr/drivers/debug/spinex-bus-error.c` | IRQ 5: pop to msgq; workqueue `LOG_WRN` 1 s |
| `zephyr/include/zephyr/drivers/spinex_bus_error.h` | sentinels / `spinex_bus_error_dump()` |
| `DEBUG_SPINEX_BUS_ERROR` | default y when the DT node is okay |
| `samples/bus_error` | SpinexMinimal HIL only |

**Loads:** the sentinel on the load (`devmem`, the sample scoreboard) is the
primary signal. Completing the cycle is the point; logging is extra.

**HIL sample:** unconditional `printk` for the five sentinels and `PASS` /
`FAIL`. Do not gate that on `LOG_WRN` (`CONFIG_LOG` may be off). Overlay disables
UDC, I2C, flash-controller, identity, and `ctrl0` so those drivers do not fill
the FIFO before `main()`. `k_sys_fatal_error_handler` dumps occupancy if a trap
still occurs (for example an iBus illegal fetch). Those stimulus loads complete;
they do not trap. Shell `bus_error clear` writes occupancy (flush FIFO; on this RTL also zeros captured,
dropped, and per-channel counts) and dropped. Popping occupancy to 0 does not by
itself zero the counters — that takes the occupancy **write**.

**Product image (usb_uvc_dual / dual_imx219):** ISR pops the FlowLogger into a
msgq only — no `LOG_*` there (that starved boot). A system workqueue item
emits `LOG_WRN` at most once per second with the first record plus occupancy /
dropped, and `LOG_DBG` for the rest. Quiet with `CONFIG_DEBUG_DRIVER_LOG_LEVEL`
(`OFF` / `ERR`). `spinex_bus_error_dump()` is an explicit `printk` dump for
shell/fatal, not the default path.

## Build / program (camera)

Spinal: `make gen TOP=dual_imx219` in `tinyclunx33_internal`. Copy
`spinal/hw/gen/dual_imx219/*.{v,sv,sdc,h,overlay}` into the Radiant design
`priv-tvai-usb` worktree `rtl/designs/dual_imx219/dual_imx219/` (do not mix the
`feature/rcc` DPHY tree). Bitfile: `make bitfile designs/dual_imx219` under
that worktree `rtl/`. Output:
`rtl/build/dual_imx219/impl/dual_imx219_impl.bit`.

Zephyr: `tinyclunx33@rev2/rtl_1_2`, sample `usb_uvc_dual`,
`CONFIG_DEBUG_SPINEX_BUS_ERROR=y`. West `ZEPHYR_EXTRA_MODULES` on the bus-error
worktree fails (`tinyclunx33` board defined twice); the west manifest still
points at dirty `tvai/priv-tvai-usb`. Build from that tree with the driver
copied, or fix the board uniqueness before using the worktree as the module.

Program SPI on `tvai-rpi5-02`: bitstream with **no** `-o`, `zephyr.bin` at
`0x200000`, then **GPIO20 power-cycle**. Do not `tvai-lab flash --sram` for this
image (releases PROGn and reloads SPI UVC from the previous slot).

SpinexMinimal HIL stays on SRAM + `samples/bus_error`; see
`docs/bus-error-working-context.md`.
