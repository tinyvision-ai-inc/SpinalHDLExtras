OUTPUT_ARCH(riscv)
ENTRY(_start)
SECTIONS {
  . = 0x20200000;
  .text : { *(.text*) }
  /DISCARD/ : { *(.note*) *(.riscv.attributes) *(.eh_frame*) }
}
