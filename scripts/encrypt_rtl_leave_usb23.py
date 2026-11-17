#!/usr/bin/env python3
"""Partial-encrypt Spinal/RTL netlists for Lattice USB23 interface capture.

Wraps `` `pragma protect `` around everything *except* Lattice USB23 HIP
instances (and any ``module USB23`` blackbox body if present). Soft logic
such as ``USB23Wrapper`` stays protected; only the ``USB23 #(...) ...;``
instantiation stays clear so Lattice can observe the core pin interface.

Then runs Radiant ``encrypt_hdl`` (``$RADIANT_HOME/ispfpga/bin/lin64/encrypt_hdl``,
same CLI as the docs' Tcl form) with the install ``key.txt``.

Reuse: module-port helpers from ``wrap_rtl_for_ipk.py`` (IPK full-file wrap).
This script is for Lattice delivery with a USB23 carve-out, not IPK packaging.

Example::

    ./encrypt_rtl_leave_usb23.py --top my_soc \\
        /path/to/my_soc.v -o /path/to/my_soc_enc.v

    # pragma wrap only (no encrypt_hdl):
    ./encrypt_rtl_leave_usb23.py --top my_soc --wrap-only in.v -o in_wrapped.v
"""

from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

from wrap_rtl_for_ipk import find_module_end, find_module_start

DATA_METHOD = '`pragma protect data_method="aes256-cbc"\n'
BEGIN = "`pragma protect begin\n"
END = "`pragma protect end\n"

# Instantiation may be preceded by (* syn_keep ... *) on the same line.
USB23_INST_START = re.compile(
    r"(?m)^[ \t]*(?:\(\*[^*]*\*\)\s*)*USB23\b(?!\w)"
)
MODULE_USB23 = re.compile(r"(?m)^[ \t]*module\s+USB23\b")


def default_radiant_home() -> Path | None:
    env = os.environ.get("RADIANT_HOME") or os.environ.get("FOUNDRY")
    if env:
        p = Path(env)
        # FOUNDRY is often .../ispfpga
        if (p / "ispfpga" / "data" / "key.txt").is_file():
            return p
        if p.name == "ispfpga" and (p / "data" / "key.txt").is_file():
            return p.parent
    root = Path("/opt/lscc/radiant")
    if not root.is_dir():
        return None
    versions = sorted(
        (d for d in root.iterdir() if d.is_dir()),
        key=lambda d: d.name,
        reverse=True,
    )
    for d in versions:
        if (d / "ispfpga" / "data" / "key.txt").is_file():
            return d
    return None


def default_key_path(radiant_home: Path | None) -> Path | None:
    if radiant_home is None:
        return None
    return radiant_home / "ispfpga" / "data" / "key.txt"


def skip_balanced(text: str, open_idx: int) -> int:
    """Return index just past the matching closer for text[open_idx] in '([{'."""
    open_ch = text[open_idx]
    close_ch = {"(": ")", "[": "]", "{": "}"}[open_ch]
    depth = 0
    i = open_idx
    n = len(text)
    while i < n:
        c = text[i]
        if c == open_ch:
            depth += 1
        elif c == close_ch:
            depth -= 1
            if depth == 0:
                return i + 1
        elif c == '"':
            i += 1
            while i < n and text[i] != '"':
                if text[i] == "\\" and i + 1 < n:
                    i += 1
                i += 1
        elif c == "'":
            # Verilog 'b0 / 'hFF — skip simple escaped tick forms
            pass
        i += 1
    raise ValueError(f"unbalanced {open_ch!r} starting at {open_idx}")


def find_usb23_instances(text: str) -> list[tuple[int, int]]:
    """Return [start, end) of each USB23 blackbox instantiation."""
    spans: list[tuple[int, int]] = []
    for m in USB23_INST_START.finditer(text):
        line_start = text.rfind("\n", 0, m.start()) + 1
        prefix = text[line_start : m.start()].strip()
        if prefix.startswith("module"):
            continue
        i = m.end()
        while i < len(text) and text[i].isspace():
            i += 1
        if i < len(text) and text[i] == "#":
            while i < len(text) and text[i].isspace():
                i += 1
            # optional whitespace already consumed; find '('
            j = i + 1
            while j < len(text) and text[j].isspace():
                j += 1
            if j >= len(text) or text[j] != "(":
                raise ValueError(f"USB23 # without parameter list at {m.start()}")
            i = skip_balanced(text, j)
        while i < len(text) and text[i].isspace():
            i += 1
        # instance name
        if i >= len(text) or not (text[i].isalnum() or text[i] == "_"):
            raise ValueError(f"USB23 instance name missing at {m.start()}")
        while i < len(text) and (text[i].isalnum() or text[i] == "_"):
            i += 1
        while i < len(text) and text[i].isspace():
            i += 1
        if i >= len(text) or text[i] != "(":
            raise ValueError(f"USB23 port list missing at {m.start()}")
        end = skip_balanced(text, i)
        while end < len(text) and text[end].isspace():
            end += 1
        if end >= len(text) or text[end] != ";":
            raise ValueError(f"USB23 instance not terminated with ';' at {m.start()}")
        end += 1
        spans.append((m.start(), end))
    return spans


def find_usb23_modules(text: str) -> list[tuple[int, int]]:
    """Return [start, end) of each ``module USB23`` … ``endmodule`` body."""
    spans: list[tuple[int, int]] = []
    for m in MODULE_USB23.finditer(text):
        rest = text[m.start() :]
        end_m = re.search(r"(?m)^[ \t]*endmodule\b", rest)
        if not end_m:
            raise ValueError("module USB23 without endmodule")
        spans.append((m.start(), m.start() + end_m.end()))
    return spans


def clear_spans(text: str) -> list[tuple[int, int]]:
    """Sorted, non-overlapping regions that must stay plaintext (USB23)."""
    spans = find_usb23_instances(text) + find_usb23_modules(text)
    spans.sort()
    if not spans:
        raise ValueError(
            "no USB23 instantiation or module USB23 found — refusing to encrypt all RTL"
        )
    merged: list[tuple[int, int]] = []
    for s, e in spans:
        if merged and s < merged[-1][1]:
            raise ValueError(f"overlapping USB23 clear spans at {s}")
        merged.append((s, e))
    return merged


def wrap_leave_usb23(text: str, top: str) -> str:
    """Insert protect pragmas; leave USB23 HIP clear."""
    if "`pragma protect begin" in text or "`pragma protect end" in text:
        raise ValueError("RTL already contains `pragma protect begin/end")

    clears = clear_spans(text)
    module_end = find_module_end(text, top)

    # Header + top ports stay clear; body is protected with USB23 carve-outs.
    out: list[str] = [DATA_METHOD, text[:module_end], "\n", BEGIN]
    cursor = module_end
    for s, e in clears:
        if s < module_end:
            raise ValueError("USB23 clear span overlaps top module port list")
        out.append(text[cursor:s])
        out.append(END)
        out.append(text[s:e])
        if not text[s:e].endswith("\n"):
            out.append("\n")
        out.append(BEGIN)
        cursor = e
    out.append(text[cursor:])
    if not text.endswith("\n"):
        out.append("\n")
    out.append(END)
    return "".join(out)


def default_encrypt_hdl(radiant_home: Path | None) -> Path | None:
    if radiant_home is None:
        return None
    p = radiant_home / "ispfpga" / "bin" / "lin64" / "encrypt_hdl"
    return p if p.is_file() else None


def run_encrypt_hdl(
    *,
    wrapped: Path,
    output: Path,
    key: Path,
    radiant_home: Path,
    encrypt_hdl: Path | None = None,
) -> None:
    """Run Radiant ``encrypt_hdl`` CLI (same tool as the Tcl command).

    Requires ``FOUNDRY=$RADIANT_HOME/ispfpga`` so the tool finds msgindex.xml.
    """
    if not key.is_file():
        raise FileNotFoundError(f"encryption key not found: {key}")

    tool = encrypt_hdl or default_encrypt_hdl(radiant_home)
    if tool is None or not tool.is_file():
        raise FileNotFoundError(
            f"encrypt_hdl not found under {radiant_home}/ispfpga/bin/lin64"
        )

    env = os.environ.copy()
    foundry = radiant_home / "ispfpga"
    env["FOUNDRY"] = str(foundry)
    env["RADIANT_HOME"] = str(radiant_home)
    lib_dirs = [
        str(foundry / "bin" / "lin64"),
        str(radiant_home / "bin" / "lin64"),
    ]
    prev = env.get("LD_LIBRARY_PATH", "")
    env["LD_LIBRARY_PATH"] = os.pathsep.join([*lib_dirs, prev] if prev else lib_dirs)

    cmd = [
        str(tool),
        "-k",
        str(key),
        "-o",
        str(output.resolve()),
        str(wrapped.resolve()),
    ]
    proc = subprocess.run(cmd, capture_output=True, text=True, env=env, check=False)
    if proc.returncode != 0:
        sys.stderr.write(proc.stdout)
        sys.stderr.write(proc.stderr)
        raise RuntimeError(f"encrypt_hdl failed (rc={proc.returncode}): {' '.join(cmd)}")
    if not output.is_file():
        sys.stderr.write(proc.stdout)
        sys.stderr.write(proc.stderr)
        raise RuntimeError(f"encrypt_hdl produced no output at {output}")


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("--top", required=True, help="Top module name (e.g. my_soc)")
    parser.add_argument(
        "rtl",
        type=Path,
        help="Plaintext Verilog input (must contain USB23 instantiation)",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="Encrypted output (.v). Default: <input>_enc.v",
    )
    parser.add_argument(
        "--wrapped",
        type=Path,
        help="Optional path to write the pragma-wrapped (still plaintext) RTL",
    )
    parser.add_argument(
        "--wrap-only",
        action="store_true",
        help="Only insert pragmas; do not run encrypt_hdl",
    )
    parser.add_argument(
        "-k",
        "--key",
        type=Path,
        help="Radiant key.txt (default: $RADIANT_HOME/ispfpga/data/key.txt)",
    )
    parser.add_argument(
        "--radiant-home",
        type=Path,
        help="Radiant install root (default: $RADIANT_HOME or newest /opt/lscc/radiant/*)",
    )
    parser.add_argument(
        "--encrypt-hdl",
        type=Path,
        help="Path to encrypt_hdl binary (default: $RADIANT_HOME/ispfpga/bin/lin64/encrypt_hdl)",
    )
    args = parser.parse_args()

    text = args.rtl.read_text()
    # Ensure top exists early
    find_module_start(text, args.top)
    wrapped_text = wrap_leave_usb23(text, args.top)

    n_inst = len(find_usb23_instances(text))
    n_mod = len(find_usb23_modules(text))
    print(
        f"Wrapped {args.rtl}: left clear {n_inst} USB23 instance(s)"
        + (f", {n_mod} module USB23" if n_mod else ""),
        file=sys.stderr,
    )

    out = args.output
    if out is None:
        out = args.rtl.with_name(args.rtl.stem + "_enc.v")

    if args.wrap_only:
        dest = args.wrapped or out
        dest.write_text(wrapped_text)
        print(f"Wrote wrapped (plaintext) RTL → {dest}", file=sys.stderr)
        return 0

    radiant_home = args.radiant_home or default_radiant_home()
    if radiant_home is None:
        raise SystemExit(
            "Could not locate Radiant install. Set RADIANT_HOME or pass --radiant-home."
        )
    key = args.key or default_key_path(radiant_home)
    if key is None or not key.is_file():
        raise SystemExit(f"encryption key not found (pass -k): {key}")

    with tempfile.TemporaryDirectory(prefix="usb23_partial_enc_") as td:
        wrapped_path = Path(td) / (args.rtl.stem + "_wrapped.v")
        if args.wrapped:
            wrapped_path = args.wrapped
        wrapped_path.write_text(wrapped_text)
        run_encrypt_hdl(
            wrapped=wrapped_path,
            output=out,
            key=key,
            radiant_home=radiant_home,
            encrypt_hdl=args.encrypt_hdl,
        )

    # Sanity: USB23 instance text should still appear in clear in output
    enc = out.read_text(errors="replace")
    if not USB23_INST_START.search(enc):
        raise SystemExit(
            f"encrypted output {out} has no clear USB23 instance — abort"
        )
    if "pragma protect begin_protected" not in enc and "key_block" not in enc:
        raise SystemExit(f"encrypted output {out} looks unencrypted — abort")

    print(f"Encrypted → {out}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
