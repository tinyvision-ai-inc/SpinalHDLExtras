"""Nexus GPLL frequency model from Lattice FPGA-TN-02095-2.7.

Section 14.7 (fractional-N, 12-bit)::

    f_OUT = f_CLKI * (N + F/4096) / (M * O)

F is an integer in 0..4095. O is the output divider, 1..128.
Appendix C: the output-divider register is one less than O
(``divide value = DIVA + 1``), and ``lmmi_ssc_f_code[14:0]`` holds the
fractional part of the feedback divider.

The technical note does not draw the 12-bit F inside that 15-bit field.
Radiant's PLL IP does, in ``ip/lifcl/pll/plugin/plugin.py``::

    SSC_F_CODE = f"{F:012b}" + "000"

which is ``F << 3``. Read back, ``(code >> 3) / 4096`` is the §14.7 fraction.

Integer mode follows §14.4.2 (the feedback divider multiplies the clock it
samples) and the same IP::

    f_VCO = f_CLKI * N * O_fb / M
    f_OUT = f_VCO / O

CrossLink-NX datasheet Table 3.34: fPFD is 18–500 MHz integer, 18–100 MHz
with fractional-N; fVCO is 800–1600 MHz; fOUT is 6.25–800 MHz.
"""

from __future__ import annotations

FRAC_DENOM = 4096
SSC_F_BITS = 15
SSC_F_SHIFT = SSC_F_BITS - FRAC_DENOM.bit_length() + 1  # 3


def pack_ssc_f(frac: int) -> int:
    """12-bit catalog fraction F -> 15-bit SSC_F_CODE, Lattice IP packing."""
    if not 0 <= frac < FRAC_DENOM:
        raise ValueError(f"F={frac} is outside 0..{FRAC_DENOM - 1}")
    code = frac << SSC_F_SHIFT
    if code >= (1 << SSC_F_BITS):
        raise ValueError(f"SSC_F_CODE {code} does not fit in {SSC_F_BITS} bits")
    return code


def unpack_ssc_f(code: int) -> int:
    """15-bit SSC_F_CODE -> 12-bit F. Low 3 bits are the IP's trailing zeros."""
    if code & ((1 << SSC_F_SHIFT) - 1):
        raise ValueError(f"SSC_F_CODE 0b{code:015b} is not a 12-bit F left-aligned")
    return code >> SSC_F_SHIFT


def fout_fractional(f_clki: float, m: int, n: int, frac: int, o: int) -> float:
    """§14.7. ``frac`` is the 12-bit F, not the raw 15-bit register."""
    return f_clki * (n + frac / FRAC_DENOM) / (m * o)


def fout_integer(f_clki: float, m: int, n: int, o_fb: int, o: int) -> float:
    """Feedback taken after output divider O_fb. §14.4.2 and the Radiant IP."""
    return f_clki * n * o_fb / (m * o)


def _mhz(hz: float) -> str:
    return f"{hz / 1e6:.6f} MHz"


def check_flir(scala_shift: int) -> None:
    """75 MHz and 90 MHz from a 60 MHz reference, the flir_uab YAML clocks."""
    f_ref = 60e6
    targets = (75e6, 90e6)
    # Scala fractional search (units of 1/4096): VCO 1350, M=1, N=22, F=2048,
    # output dividers 18 and 15 (DIVA=17, DIVB=14).
    m, n, frac, outputs = 1, 22, 2048, (18, 15)

    old_code = frac  # to_bin_string(F, 15): zero-padded on the left
    old_f = unpack_ssc_f(old_code)  # Lattice reads the top 12 bits, so F becomes 256
    old_hz = tuple(fout_fractional(f_ref, m, n, old_f, o) for o in outputs)
    print(f"old Scala SSC_F_CODE 0b{old_code:015b} decodes as F={old_f}/4096:")
    for target, hz in zip(targets, old_hz):
        print(f"  want {_mhz(target):>14}  model {_mhz(hz):>14}")

    if scala_shift != SSC_F_SHIFT:
        raise SystemExit(f"Scala ssc_f_code_shift {scala_shift} != model {SSC_F_SHIFT}")
    new_code = frac << scala_shift
    if new_code != pack_ssc_f(frac):
        raise SystemExit("Scala shift does not match Lattice IP left-align")
    new_hz = tuple(fout_fractional(f_ref, m, n, unpack_ssc_f(new_code), o) for o in outputs)
    print(f"new Scala SSC_F_CODE 0b{new_code:015b} decodes as F={unpack_ssc_f(new_code)}/4096:")
    for target, hz in zip(targets, new_hz):
        print(f"  want {_mhz(target):>14}  model {_mhz(hz):>14}")
        if abs(hz - target) > 1.0:
            raise SystemExit("fractional encode does not hit the YAML clocks")

    # Integer solution Scala keeps now that solve_non_fractional is not discarded.
    # PFD = 60/2 = 30 MHz (datasheet floor is 18). Feedback is the 90 MHz output.
    int_m, int_n, o_fb = 2, 3, 15
    int_outputs = (18, 15)  # 75 MHz, 90 MHz
    print("integer solution (feedback on the 90 MHz output):")
    for target, o in zip(targets, int_outputs):
        hz = fout_integer(f_ref, int_m, int_n, o_fb, o)
        print(f"  want {_mhz(target):>14}  model {_mhz(hz):>14}")
        if abs(hz - target) > 1.0:
            raise SystemExit("integer solution does not hit the YAML clocks")
    pfd = f_ref / int_m
    if not 18e6 <= pfd <= 500e6:
        raise SystemExit(f"integer PFD {_mhz(pfd)} is outside the datasheet range")


def scala_shift(path: str) -> int:
    text = open(path, encoding="utf-8").read()
    if "solution = None" in text:
        raise SystemExit("PLL.scala still discards the integer solution")
    # Shift is computed from the two constants. Recompute it the same way.
    denom = bits = None
    for line in text.splitlines():
        s = line.strip()
        if s.startswith("val clkfb_frac_denom"):
            denom = int(s.split("=")[-1].strip())
        elif s.startswith("val ssc_f_code_bits"):
            bits = int(s.split("=")[-1].strip())
    if denom is None or bits is None:
        raise SystemExit("could not read fraction constants from PLL.scala")
    return bits - (denom.bit_length() - 1)


if __name__ == "__main__":
    import pathlib

    here = pathlib.Path(__file__).resolve().parent / "PLL.scala"
    shift = scala_shift(str(here))
    print(f"Scala ssc_f_code_shift = {shift}")
    check_flir(shift)
    print("ok")
