# SpineX SPI / XIP flash controller

APB NOR controller used by SpineX (`XipFlashPlugin`): command FIFO for
program/erase, plus an XIP FSM for execute-in-place. XIP addresses are remapped
by `XIP_OFFSET` (`0x4C`); command-mode SPI is never remapped.

Generated SoC description: `{design}.overlay` node
`flashctrl: flash-controller@e0001000` (from `DeviceTree.generate`).
Zephyr binding: `tinyvision,flash`. First `reg` is the spanning APB window
(`base`, `0x5c`) so `DT_INST_REG_ADDR(0)` is the controller base.

## Memory map

| Region | CPU address | Size |
|---|---|---|
| APB CSRs | `0xe0001000` | `0x5c` (APB decode is 1 KiB) |
| XIP window | `0x20000000` | 16 MiB |

`nor = cpu + XIP_OFFSET - 0x20200000`. Reset `XIP_OFFSET` is `0x00200000`
(slot0), so `0x20200000` fetches NOR `0x200000`. Offset bits `[15:0]` are tied
off (64 KiB alignment). XIP stays 3-byte (`0xEB`).

SPI modes: **0** = 1-bit full duplex, **1** = 4-bit (quad). Timers are 12-bit.

W = write-only (readback 0), R = read-only, RW = both.

## Command / status

| Off | Name | Acc | Reset | Description |
|---|---|---|---|---|
| `0x00` | DATA | W/R | — | **W:** push one SPI cmd. `[7:0]` payload; `[8]` write; `[9]` read; `[11]` CS (kind). CS: `[7]`=assert, `[0]`=ss id (always 0). **R:** pop one rsp byte; `[7:0]` data; `[31]`=`1` if FIFO empty. Read pops even if empty. |
| `0x04` | BUFFER | R | 0 | `[15:0]` cmd FIFO free (depth 32). `[31:16]` rsp FIFO occupancy. |
| `0x08` | CONFIG | W | 0 | `[0]` CPOL, `[1]` CPHA, `[4]` SPI mode id. |
| `0x0C` | INTERRUPT | RW | 0 | `[0]` cmd-irq en, `[1]` rsp-irq en. **R:** `[8]` cmd irq, `[9]` rsp irq, `[16]` SPI `cmd.valid`. IRQ pin exists in the IP but is not wired to the CPU on SpineX. |

DATA recipes:

- write byte: `data \| (1<<8)`
- read byte: `(1<<9)`
- CS assert: `ssid \| 0x80 \| (1<<11)`
- CS deassert: `ssid \| (1<<11)`

## Timing / chip-select

| Off | Name | Acc | Reset | Description |
|---|---|---|---|---|
| `0x20` | CLK_DIVIDER | W | 0 | `sclkToggle`. SCLK half-period in ctrl clocks. 0 = fastest. |
| `0x24` | SS_SETUP | W | 0 | CS setup ticks before SCLK. |
| `0x28` | SS_HOLD | W | 0 | CS hold ticks after SCLK. |
| `0x2C` | SS_DISABLE | W | 0 | CS high time between transactions. |
| `0x30` | SS_ACTIVE | W | 0 | CS active-high mask (`ssWidth=1`). 0 = active low. |

## XIP

| Off | Name | Acc | Reset | Description |
|---|---|---|---|---|
| `0x40` | XIP_ENABLE | W | 1 | Latched enable. FSM never reads this. XIP window always hits the XIP FSM. |
| `0x44` | XIP_INSTR | W | `0x02FF01EB` | `[7:0]` opcode (`0xEB`). `[8]` send opcode (1). `[23:16]` dummy (`0xFF`). `[27:24]` dummy beats (2). |
| `0x48` | XIP_MOD | W | `0x01010100` | Mode id per phase: `[7:0]` instruction (0), `[15:8]` address (1), `[23:16]` dummy (1), `[31:24]` payload (1). |
| `0x4C` | XIP_OFFSET | RW | `0x00200000` | NOR image base. `[15:0]` RAZ. XIP only. Write ignored unless XIP idle (`pending == 0 && !cmd.valid`). Overflow (NOR range outside 16 MiB) → bus error on the fetch, no SPI. |

The APB write to `0x4C` always completes (`PREADY`). It does not take an
exception. A later XIP fetch uses the new offset only after a successful idle
write; I-cache lines are not flushed (`fence.i` required).

## Wide DATA aliases

| Off | Name | Acc | Description |
|---|---|---|---|
| `0x50` | DATA32 | W | Push write-data cmd (no need to set bit 8). Payload still 8-bit. |
| `0x54` | DATA32_RW | W | Push write+read (full duplex). |
| `0x58` | DATA32_RSP | R | Pop rsp; `[7:0]` data, no empty flag in bit 31. Still pops. |

## Slot switch

Run from RAM → write `XIP_OFFSET` → read it back → `fence.i` → jump to XIP.

## Device tree (generated overlay)

```dts
flashctrl: flash-controller@e0001000 {
    compatible = "tinyvision,flash";
    status = "okay";
    reg = <0xe0001000 0x5c
           0xe0001000 0x4
           0xe0001004 0x4
           0xe0001008 0x4
           0xe000100c 0x4
           0xe0001020 0x4
           0xe0001024 0x4
           0xe0001028 0x4
           0xe000102c 0x4
           0xe0001030 0x4
           0xe0001040 0x4
           0xe0001044 0x4
           0xe0001048 0x4
           0xe000104c 0x4
           0xe0001050 0x4
           0xe0001054 0x4
           0xe0001058 0x4>;
    reg-names = "base", "data", "buffer", "config", "interrupt",
                "clk_divider", "ss_setup", "ss_hold", "ss_disable",
                "ss_active", "xip_enable", "xip_instr", "xip_mod",
                "xip_offset", "data32", "data32_rw", "data32_rsp";

    flash0: flash@20000000 {
        compatible = "soc-nv-flash";
        #address-cells = <1>;
        #size-cells = <0>;
        reg = <0x20000000 0x1000000>;
        write-block-size = <32>;
        erase-block-size = <0x1000>;
    };
};
```
