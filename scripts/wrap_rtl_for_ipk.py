#!/usr/bin/env python3
"""Insert Lattice IP packaging attributes for ippackc."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

LSC_ATTR = "(* LSC_IP_SC_HT_tvai_usb_cnx *)"


def find_module_start(text: str, top: str) -> int:
    match = re.search(rf"^module\s+{re.escape(top)}\b", text, re.MULTILINE)
    if not match:
        match = re.search(r"^module\s+\w+", text, re.MULTILINE)
    if not match:
        raise ValueError(f"module {top!r} not found")
    return match.start()


def find_module_end(text: str, top: str) -> int:
    start = find_module_start(text, top)
    after = text[start:]
    end = re.search(r"\);", after, re.MULTILINE)
    if not end:
        raise ValueError("module declaration terminator ');' not found")
    return start + end.end()


def _block_start(text: str, module_start: int) -> int:
    """Start of a module including the attribute lines directly above it."""
    start = module_start
    while start > 0:
        prev = text.rfind("\n", 0, start - 1) + 1
        if not text[prev:start].lstrip().startswith("(*"):
            break
        start = prev
    return start


def hoist_top(text: str, top: str) -> str:
    """Move the top module ahead of every other module.

    Only the text after the top module's port list is encrypted, and Yosys
    writes modules in name order, so without this most of the design stays
    readable.
    """
    first = re.search(r"^module\s+\w+", text, re.MULTILINE)
    top_start = find_module_start(text, top)
    if not first or first.start() == top_start:
        return text
    end = re.search(r"^endmodule\b[^\n]*\n?", text[top_start:], re.MULTILINE)
    if not end:
        raise ValueError(f"endmodule of {top!r} not found")
    block_start = _block_start(text, top_start)
    block_end = top_start + end.end()
    block = text[block_start:block_end]
    rest = text[:block_start] + text[block_end:]
    insert = _block_start(rest, first.start())
    return rest[:insert] + block.rstrip("\n") + "\n\n" + rest[insert:]


def wrap_rtl(text: str, top: str, *, encrypt: bool = True) -> str:
    if encrypt and "`pragma protect begin" in text:
        raise ValueError("RTL already wrapped for IP packaging")

    if encrypt:
        text = hoist_top(text, top)

    module_start = find_module_start(text, top)
    module_end = find_module_end(text, top)

    text = text[:module_start] + f"`define SYNTHESIS\n{LSC_ATTR}\n" + text[module_start:]
    module_end += len("`define SYNTHESIS\n") + len(LSC_ATTR) + 1

    if encrypt:
        protect_insert = "\n`pragma protect begin\n"
        text = text[:module_end] + protect_insert + text[module_end:]
        if "`pragma protect end" not in text:
            text = text.rstrip() + "\n`pragma protect end\n"
    return text


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--top", required=True, help="Top module name")
    encrypt_kwargs = {
        "dest": "encrypt",
        "default": True,
        "help": "Wrap RTL with `pragma protect for ippackc encryption (default: on)",
    }
    if hasattr(argparse, "BooleanOptionalAction"):
        parser.add_argument("--encrypt", action=argparse.BooleanOptionalAction, **encrypt_kwargs)
    else:
        parser.add_argument("--encrypt", action="store_true", **encrypt_kwargs)
        parser.add_argument("--no-encrypt", dest="encrypt", action="store_false")
    parser.add_argument("rtl", type=Path, help="RTL file to modify in place")
    args = parser.parse_args()

    args.rtl.write_text(wrap_rtl(args.rtl.read_text(), args.top, encrypt=args.encrypt))
    print(f"Wrapped {args.rtl}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
