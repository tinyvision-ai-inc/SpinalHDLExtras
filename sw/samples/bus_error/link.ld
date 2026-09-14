OUTPUT_ARCH(riscv)
ENTRY(_start)
SECTIONS {
  . = 0x20200000;
  .text : { *(.text*) }
}
