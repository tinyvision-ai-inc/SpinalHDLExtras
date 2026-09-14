# Bus-error HIL — working context

Last updated: 2026-09-08. Locked design stands (no PMB `rsp.error`, no `mcause` 7, FlowLogger at `0xe0008000`, IRQ 5). Channel names in generated RTL are `axi_usb_bus_error` / `axi_mmi_bus_error` (not `usb_axi_err`).

Two DUTs. Do not mix load methods.

## Camera SoC (`dual_imx219`) — HIL 2026-09-08 (congestion RTL)

- Machine: **`tvai-rpi5-02`**. SPI bit (no `-o`), `zephyr.bin` at **`0x200000`**, then GPIO20 power-cycle. Do **not** `--sram`.
- RTL worktree: `~/zephyrproject/tvai/priv-tvai-usb-bus-error`. Bitfile: `rtl/build/dual_imx219/impl/dual_imx219_impl.bit` (AxiRamOrMiss, FlowCCByToggle, logger `localDepth=0`).
- Mapper: LUT 15448, regs 17410 (62.97%), EBR 26.56%, LRAM 60%, dist-RAM 235. PAR ~8 min, 0 unrouted. TWR setup still open (worst ~-0.205 ns / 3 endpoints at 85 C). PAR estimated WNS -1.643 ns; trust TWR.
- Firmware: `usb_cdc_shell` (not `usb_uvc_dual`: dirty `feature/rcc` `uvc.c` needs missing `zephyr/video/controls.h`). West: `-DZEPHYR_MODULES=...;priv-tvai-usb-bus-error` so the board is not defined twice. Bin: `~/zephyrproject/build/bus-error-cdc-shell/zephyr/zephyr.bin`. `add_subdirectory_ifdef(CONFIG_DEBUG_DRIVER debug)` (not `CONFIG_DEBUG`) so `spinex-bus-error.c` links.
- HIL: `run_smoke.sh --force-bitstream` (SPI-head probe drops FTDI; a second ssh then misses `ttyUSB0`). Boot: `Booting Zephyr OS build v4.4.0-2159-ge6aa31181e1c`. `devmem 0xb1020000` → `Read value 0xdeadbeef`. Harness `--commands` expect `DEADBEEF` is case-sensitive vs Zephyr `0xdeadbeef`.
- Logger under USB bring-up: `occupancy=0` with large `dropped` / `msgq_drop` (one-deep CDC + localDepth 0 cannot keep a storm).
- **Do not** poke SpinexMinimal windows on this image. `devmem 0xe0008504` / `8600` / `8700` / `8800` all return `0xDEADBEEF` and bump `[0]` (`pmb_decode_miss_d`). `0x50000000` is the PMB miss probe; `0xb1020000` is MMI AXI (`[3]`). `[4]` is `wb_decode_miss` and can sit in the tens of thousands with no poke. See `docs/bus-error-logger.md`.

## SpinexMinimal — done

- Machine: `tvai-rpi5-01-1`.
- Image: `radiant/spinex_demo/impl_1/spinex_demo_impl_1.bit` (ROM = `samples/bus_error`).
- Clock: 60 MHz pad `clk`. After SRAM, **do not power-cycle**. Restore with `tvai-lab -m tvai-rpi5-01-1 power cycle`.

## How to load SpinexMinimal

Need **both** `ttyUSB0` (JTAG) and `ttyUSB1` (UART). If only `ttyUSB1` is left, power-cycle first.

`tvai-lab flash --sram` asserts PROGn, programs SRAM, **releases PROGn**. Nexus then loads **SPI flash** (dual_imx219 UVC). Do not use that path for this image.

Working load:

1. Power cycle if `ttyUSB0` is missing.
2. Open UART first (`/dev/ttyUSB1` @ 115200). Re-open on FTDI reset.
3. `ecpprog -k 3 -S spinex_demo_impl_1.bit` with PROGn already released (`pinctrl` 21 `ip pu`).
4. Do not pulse PROGn after.

## Timeout RTL

`PipelinedMemoryBusTimeout` holds `rsp.valid` + sentinel the cycle **after** `cmd.ready` (`rspHold`), same as `PmbMissCompleter`. Same-cycle `forceCmd` rsp is dropped by `rspPipe()` on dBus, so the CPU saw stale `SPNX` (`0x53504e58`).

Sim: `pmbReadAfterCmd` (no same-cycle take). `BusErrorLogTest` all 13 green.

## HIL (2026-09-08, after rspHold)

Zephyr `v4.4.0-2159-ge6aa31181e1c`:

```
=== bus-error HIL ===
  miss: deadbeef (expect deadbeef)
  unimpl: a5a5a5a5 (expect a5a5a5a5)
  slverr: 5a5a5a5a (expect 5a5a5a5a)
  proto: deadbeef (expect deadbeef)
  timeout: cafed00d (expect cafed00d)
occupancy after stim=0
PASS (0 mismatches)
```

DUT restored with power cycle after this run.
