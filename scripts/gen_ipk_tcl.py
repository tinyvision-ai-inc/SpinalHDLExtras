#!/usr/bin/env python3
"""Generate Lattice ippackc Tcl from a Verilog/SystemVerilog top module."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


PORT_RE = re.compile(
    r"(?P<dir>input|output|inout)\s+"
    r"(?:(?:wire|reg|logic)\s+)?"
    r"(?:(?:\[(?P<msb>\d+):(?P<lsb>\d+)\]\s+)?)?"
    r"(?P<name>\w+)",
    re.IGNORECASE,
)


def extract_module_ports(text: str) -> list[tuple[str, str, tuple[int, int] | None]]:
    match = re.search(r"module\s+\w+\s*\((.*?)\)\s*;", text, re.DOTALL | re.MULTILINE)
    if not match:
        raise ValueError("Could not find module port list")

    ports: list[tuple[str, str, tuple[int, int] | None]] = []
    seen: set[str] = set()
    for port in PORT_RE.finditer(match.group(1)):
        name = port.group("name")
        if name in seen:
            continue
        seen.add(name)
        direction = port.group("dir").lower()
        if port.group("msb") is not None:
            msb = int(port.group("msb"))
            lsb = int(port.group("lsb"))
            width = (max(msb, lsb), min(msb, lsb))
        else:
            width = None
        ports.append((name, direction, width))
    if not ports:
        raise ValueError("No ports parsed from module")
    return ports


def tcl_escape(value: str) -> str:
    return "{" + value.replace("\\", "\\\\").replace("{", "\\{").replace("}", "\\}") + "}"


def port_line(name: str, direction: str, width: tuple[int, int] | None) -> str:
    ippack_dir = {"input": "in", "output": "out", "inout": "inout"}[direction]
    if width is None:
        return f"ipk_add_port -name {name} -dir {ippack_dir}"
    high, low = width
    return f"ipk_add_port -name {name} -dir {ippack_dir} -range ({high},{low})"


def render_tcl(
    ports: list[tuple[str, str, tuple[int, int] | None]],
    *,
    version: str,
    display_name: str,
    category: str,
    keywords: str,
    rtl_files: list[str],
    constraint_file: str,
    doc_files: list[str],
    encrypt_rtl: bool,
) -> str:
    # run.tcl is executed with cwd=work/. Staged inputs live in ./stage; the
    # ippackc project is a fresh empty ./ipk_proj. Opening the stage directory
    # itself made Lattice append into staged docs (e.g. introduction.html).
    lines = [
        "# cwd is work/ (see ip_packager Makefile). Keep stage and project separate.",
        "set ipk_stage_dir [file normalize [file join [pwd] stage]]",
        "set ipk_proj_dir [file normalize [file join [pwd] ipk_proj]]",
        "file delete -force $ipk_proj_dir",
        "file mkdir $ipk_proj_dir",
        "ipk_open -path $ipk_proj_dir",
        "ipk_set_ip_info -type vendor -value {tinyVision.ai}",
        "ipk_set_ip_info -type library -value {ip}",
        f"ipk_set_ip_info -type name -value {tcl_escape(display_name)}",
        f"ipk_set_ip_info -type version -value {tcl_escape(version)}",
        f"ipk_set_ip_info -type category -value {tcl_escape(category)}",
        f"ipk_set_ip_info -type keywords -value {tcl_escape(keywords)}",
        "ipk_set_ip_info -type supported_platforms -value {Radiant}",
        "ipk_set_ip_info -type instantiate_once -value true",
        f"ipk_set_ip_info -type display_name -value {tcl_escape(display_name)}",
        "ipk_remove_device -family *",
        "ipk_add_device -family LIFCL -device LIFCL-33U",
    ]

    for name, direction, width in ports:
        lines.append(port_line(name, direction, width))

    for rtl in rtl_files:
        lines.append(f"ipk_add_file -type rtl -file [file join $ipk_stage_dir {rtl}]")
        if encrypt_rtl:
            lines.append(
                "ipk_set_file_property -type rtl "
                f"-file [file join $ipk_stage_dir {rtl}] "
                "-prop_name encrypt -prop_value {1}"
            )

    lines.extend(
        [
            f"ipk_add_file -type tcl_constraint -file [file join $ipk_stage_dir {constraint_file}]",
            "ipk_add_file -type eula -file [file join $ipk_stage_dir tinyVision.ai_Inc_EULA.txt]",
        ]
    )
    for doc in doc_files:
        lines.append(f"ipk_add_file -type doc -file [file join $ipk_stage_dir {doc}]")
    lines.extend(
        [
            "ipk_drc",
            "ipk_save",
            "ipk_package",
            "ipk_close",
            "",
        ]
    )
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--top", required=True, help="Top module / IP base name")
    parser.add_argument("--ports", required=True, type=Path, help="Top-level SV/V file with port list")
    parser.add_argument("--output", required=True, type=Path, help="Output Tcl path")
    parser.add_argument("--version", default="1.0.0")
    parser.add_argument("--display-name", default=None)
    parser.add_argument("--category", default="USB")
    parser.add_argument("--keywords", default=None)
    parser.add_argument("--constraint-file", default=None, help="SDC filename inside work/")
    parser.add_argument(
        "--extra-rtl",
        action="append",
        default=[],
        help="Additional RTL basenames in work/ (repeatable)",
    )
    parser.add_argument(
        "--extra-doc",
        action="append",
        default=[],
        help="Additional doc file basenames in work/ (repeatable)",
    )
    encrypt_rtl_kwargs = {
        "dest": "encrypt_rtl",
        "default": True,
        "help": "Set ippackc encrypt property on RTL files (default: on)",
    }
    if hasattr(argparse, "BooleanOptionalAction"):
        parser.add_argument("--encrypt-rtl", action=argparse.BooleanOptionalAction, **encrypt_rtl_kwargs)
    else:
        parser.add_argument("--encrypt-rtl", action="store_true", **encrypt_rtl_kwargs)
        parser.add_argument("--no-encrypt-rtl", dest="encrypt_rtl", action="store_false")
    args = parser.parse_args()

    display_name = args.display_name or args.top
    keywords = args.keywords or f"USB, {args.top}"
    constraint_file = args.constraint_file or f"{args.top}.sdc"
    rtl_files = list(args.extra_rtl) + [f"{args.top}.v"]

    text = args.ports.read_text()
    ports = extract_module_ports(text)
    tcl = render_tcl(
        ports,
        version=args.version,
        display_name=display_name,
        category=args.category,
        keywords=keywords,
        rtl_files=rtl_files,
        constraint_file=constraint_file,
        doc_files=[
            "introduction.html",
            *args.extra_doc,
        ],
        encrypt_rtl=args.encrypt_rtl,
    )
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(tcl)
    print(f"Wrote {args.output} ({len(ports)} ports)", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
