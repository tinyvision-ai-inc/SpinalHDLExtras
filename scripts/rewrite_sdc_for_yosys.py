#!/usr/bin/env python3
"""Rewrite Spinal SDC paths to match a Yosys-optimized Verilog netlist."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

from validate_sdc_paths import ModuleInfo, parse_modules, resolve_path

# Prefix keeps -hierarchical and opening {; body is brace-free tokens; suffix is }].
PATH_RE = re.compile(
    r"(\[(?:get_nets|get_ports|get_pins|get_cells)"
    r"(?:\s+-hierarchical)?\s+(?:\{)?)"
    r"([^\]\}]+?)"
    r"((?:\})?\])"
)

# Boundary ports promoted to top-level after Yosys flattening / port propagation.
TOP_PORT_ALIASES = {
    "spinex_som/spiflash_clk": "spiflash_clk",
    "spinex_som/spiflash_cs_n": "spiflash_cs_n",
    "spinex_som/spiflash_dq": "spiflash_dq",
    "spinex_som/jtag_tck": "jtag_tck",
    "jtagChain_2/jtag_tck": "jtag_tck",
    "jtagChain_2/jtag_tdi": "jtag_tdi",
    "jtagChain_2/jtag_tdo": "jtag_tdo",
    "jtagChain_2/jtag_tms": "jtag_tms",
    "spinex_som/jtagChain_2/jtag_tck": "jtag_tck",
    "spinex_som/jtagChain_2/jtag_tdi": "jtag_tdi",
    "spinex_som/jtagChain_2/jtag_tdo": "jtag_tdo",
    "spinex_som/jtagChain_2/jtag_tms": "jtag_tms",
}


def rewrite_token(
    top: str, token: str, modules: dict[str, ModuleInfo], verilog: str
) -> str:
    token = token.strip("{}")
    if token.startswith("-"):
        return token
    if token.endswith("/*"):
        suffix = "/*"
        core = token[:-2]
    elif token.endswith("*"):
        suffix = "*"
        core = token[:-1]
    else:
        suffix = ""
        core = token

    if "*" in core:
        return token

    if core == "clk" or resolve_path(top, core, modules):
        return token

    if core in TOP_PORT_ALIASES:
        mapped = TOP_PORT_ALIASES[core]
        if mapped in modules[top].ports:
            return mapped + suffix

    flat = "_".join(core.split("/"))
    if re.search(rf"\b{re.escape(flat)}\b", verilog):
        return flat + suffix

    return token


def rewrite_line(
    line: str, top: str, modules: dict[str, ModuleInfo], verilog: str
) -> str:
    def repl(m: re.Match[str]) -> str:
        prefix, body, suffix = m.group(1), m.group(2), m.group(3)
        tokens = []
        for raw in body.split():
            tokens.append(rewrite_token(top, raw, modules, verilog))
        return prefix + " ".join(tokens) + suffix

    return PATH_RE.sub(repl, line)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--top", required=True)
    ap.add_argument("--verilog", type=Path, required=True)
    ap.add_argument("--sdc", type=Path, required=True)
    ap.add_argument("--in-place", action="store_true")
    args = ap.parse_args()

    verilog = args.verilog.read_text()
    modules = parse_modules(args.verilog)
    lines_out = [
        rewrite_line(line, args.top, modules, verilog)
        for line in args.sdc.read_text().splitlines()
    ]
    text = "\n".join(lines_out) + "\n"
    if args.in_place:
        args.sdc.write_text(text)
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
