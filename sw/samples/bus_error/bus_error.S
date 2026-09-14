/* SpinexMinimal BusErrorTestPlugin windows → CPU access fault → tohost. */
  .section .text
  .globl _start
_start:
  la   t0, trap
  csrw mtvec, t0
  li   s0, 0

  /* 1: UNIMPL load @ 0xe0008504 → mcause 5 */
  li   a2, 5
  li   t1, 0xe0008504
  lw   t2, 0(t1)
  ori  s0, s0, 1

  /* 2: TIMEOUT CMD @ 0xe0008600 → mcause 5 */
  li   a2, 5
  li   t1, 0xe0008600
  lw   t2, 0(t1)
  ori  s0, s0, 2

  /* 3: SLVERR @ 0xe0008700 → mcause 5 */
  li   a2, 5
  li   t1, 0xe0008700
  lw   t2, 0(t1)
  ori  s0, s0, 4

  /* 4: UNIMPL store @ 0xe0008504 → mcause 7 */
  li   a2, 7
  li   t1, 0xe0008504
  sw   zero, 0(t1)
  ori  s0, s0, 8

  li   t1, 0xe0008900
  li   t2, 0xf
  bne  s0, t2, fail
  li   t2, 1
  sw   t2, 0(t1)
1: j 1b

fail:
  sw   s0, 0(t1)
2: j 2b

trap:
  csrr t3, mcause
  bne  t3, a2, bad
  csrr t4, mepc
  addi t4, t4, 4
  csrw mepc, t4
  mret
bad:
  li   t1, 0xe0008900
  /* high bit set + actual mcause */
  li   t2, 0x80000000
  or   t2, t2, t3
  sw   t2, 0(t1)
3: j 3b
