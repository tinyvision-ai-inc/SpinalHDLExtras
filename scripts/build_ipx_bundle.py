#!/usr/bin/env python3
"""Assemble an IPX module directory for Lattice Radiant.

RTL, constraints, and docs listed in the packaged .ipk are extracted into the
bundle. Other IPX artifacts (references, top wrapper) still come from the
Spinal / checked-in source tree.

Layout:
  {bundle}/{top}.ipx
  {bundle}/{top}.v                 (from IPK rtl/, encrypted when ENCRYPT=1)
  {bundle}/{top}_top.sv            (port wrapper from Spinal hw/gen)
  {bundle}/{top}_references.v      (from source)
  {bundle}/{top}.sdc               (from IPK constraints/)
  {bundle}/introduction.html       (from IPK doc/)
  {bundle}/Ram_1wrs.sv             (from IPK rtl/ when packaged)
"""

from __future__ import annotations

import argparse
import shutil
import sys
import xml.etree.ElementTree as ET
import zipfile
from dataclasses import dataclass


def _indent_xml(tree: ET.ElementTree, space: str = " ") -> None:
    if hasattr(ET, "indent"):
        ET.indent(tree, space=space)
from pathlib import Path

RTL_SUFFIXES = frozenset({".v", ".sv", ".vh", ".svh"})


@dataclass(frozen=True)
class IpxFile:
    name: str
    file_type: str
    stage: str | None = None


def parse_ipx_files(ipx_path: Path) -> list[str]:
    root = ET.parse(ipx_path).getroot()
    return [
        node.attrib["name"]
        for node in root.findall(".//File")
        if node.attrib.get("name")
    ]


def resolve_ipx(source_dir: Path, top: str) -> tuple[Path, Path]:
    """Return (ipx_path, directory holding files listed in the IPX manifest)."""
    sub_ipx = source_dir / top / f"{top}.ipx"
    flat_ipx = source_dir / f"{top}.ipx"
    if sub_ipx.is_file():
        return sub_ipx, source_dir / top
    if flat_ipx.is_file():
        return flat_ipx, source_dir
    raise FileNotFoundError(f"no IPX manifest for {top} under {source_dir}")


def find_file(name: str, top: str, file_dir: Path, source_dir: Path) -> Path | None:
    candidates = [
        file_dir / name,
        source_dir / name,
        source_dir / top / name,
    ]
    for candidate in candidates:
        if candidate.is_file():
            return candidate
    return None


def find_top_sv(top: str, file_dir: Path, source_dir: Path) -> Path | None:
    return find_file(f"{top}_top.sv", top, file_dir, source_dir)


def sort_ipk_rtl(names: list[str], top: str) -> list[str]:
    primary = f"{top}.v"
    rest = sorted(name for name in names if name != primary)
    return ([primary] if primary in names else []) + rest


def list_ipk_rtl_names(ipk_path: Path) -> list[str]:
    names: list[str] = []
    with zipfile.ZipFile(ipk_path) as archive:
        for member in archive.namelist():
            if not member.startswith("rtl/"):
                continue
            path = Path(member)
            if path.suffix.lower() in RTL_SUFFIXES:
                names.append(path.name)
    return names


def remove_plain_rtl_before_ipk_extract(ipk_path: Path, output_dir: Path) -> None:
    """Remove IPGen/plain RTL that will be replaced by the packaged copy from the IPK."""
    for name in list_ipk_rtl_names(ipk_path):
        path = output_dir / name
        if path.is_file():
            path.unlink()
            print(f"Removed plain RTL (replaced by IPK): {path}", file=sys.stderr)


def extract_ipk_rtl(ipk_path: Path, output_dir: Path, top: str) -> list[str]:
    extracted: list[str] = []
    with zipfile.ZipFile(ipk_path) as archive:
        for member in archive.namelist():
            if not member.startswith("rtl/"):
                continue
            path = Path(member)
            if path.suffix.lower() not in RTL_SUFFIXES:
                continue
            dst = output_dir / path.name
            dst.write_bytes(archive.read(member))
            extracted.append(path.name)
    return sort_ipk_rtl(extracted, top)


def extract_ipk_constraints(ipk_path: Path, output_dir: Path, top: str) -> list[str]:
    extracted: list[str] = []
    with zipfile.ZipFile(ipk_path) as archive:
        for member in archive.namelist():
            if not member.startswith("constraints/"):
                continue
            path = Path(member)
            if path.suffix.lower() != ".sdc":
                continue
            # Flatten to {top}.sdc for the Radiant IPX bundle layout.
            dst_name = f"{top}.sdc"
            dst = output_dir / dst_name
            dst.write_bytes(archive.read(member))
            extracted.append(dst_name)
    return extracted


def extract_ipk_docs(ipk_path: Path, output_dir: Path) -> list[str]:
    extracted: list[str] = []
    with zipfile.ZipFile(ipk_path) as archive:
        for member in archive.namelist():
            if not member.startswith("doc/"):
                continue
            path = Path(member)
            if path.suffix.lower() != ".html":
                continue
            dst = output_dir / path.name
            dst.write_bytes(archive.read(member))
            extracted.append(path.name)
    return sorted(extracted)


def ipx_file_entry(name: str, top: str) -> IpxFile:
    if name.endswith(".sdc"):
        return IpxFile(
            name,
            "tcl_constraints",
            stage=f"{name}=lse:presyn,synplify:presyn",
        )
    if name.endswith(".html"):
        return IpxFile(name, "doc")
    if name.endswith(".ldc"):
        return IpxFile(name, "timing_constraints")
    return IpxFile(name, "top_level_verilog")


def references_defines_ram(references_path: Path) -> bool:
    if not references_path.is_file():
        return False
    text = references_path.read_text(encoding="utf-8", errors="replace")
    return "module Ram_1wrs" in text


def package_file_order(name: str, top: str) -> tuple[int, str]:
    preferred = {
        f"{top}.v": 0,
        f"{top}_top.sv": 1,
        "Ram_1wrs.sv": 2,
        f"{top}_references.v": 3,
        f"{top}.sdc": 4,
        "introduction.html": 5,
    }
    return (preferred.get(name, 99), name)


def write_ipx(
    ipx_src: Path,
    output_path: Path,
    package_files: list[IpxFile],
    *,
    version: str | None = None,
) -> None:
    tree = ET.parse(ipx_src)
    root = tree.getroot()
    if version:
        root.attrib["version"] = version
    package = root.find("Package")
    if package is None:
        raise ValueError(f"{ipx_src}: missing <Package> element")

    for child in list(package):
        package.remove(child)
    for entry in package_files:
        attrs = {"name": entry.name, "type": entry.file_type}
        if entry.stage:
            attrs["stage"] = entry.stage
        ET.SubElement(package, "File", **attrs)

    _indent_xml(tree, space=" ")
    output_path.write_text(
        '<?xml version="1.0" ?>\n' + ET.tostring(root, encoding="unicode") + "\n"
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--top", required=True, help="Top-level module / IP name")
    parser.add_argument(
        "--source-dir",
        type=Path,
        required=True,
        help="Spinal hw/gen tree or checked-in $(TOP)/ directory",
    )
    parser.add_argument(
        "--ipk",
        type=Path,
        required=True,
        help="Packaged .ipk archive (RTL/constraints/docs taken from the zip)",
    )
    parser.add_argument(
        "--output",
        type=Path,
        required=True,
        help="Output bundle directory, e.g. release/foo_1.0/foo",
    )
    parser.add_argument(
        "--version",
        default=None,
        help="IP package version for RadiantModule version= (default: keep source IPX value)",
    )
    args = parser.parse_args()

    if not args.ipk.is_file():
        print(f"error: IPK not found: {args.ipk}", file=sys.stderr)
        return 1

    try:
        ipx_src, file_dir = resolve_ipx(args.source_dir, args.top)
    except FileNotFoundError as exc:
        print(f"Skipping IPX bundle: {exc}", file=sys.stderr)
        return 0

    args.output.mkdir(parents=True, exist_ok=True)

    remove_plain_rtl_before_ipk_extract(args.ipk, args.output)

    ipk_rtl = extract_ipk_rtl(args.ipk, args.output, args.top)
    if not ipk_rtl:
        print(f"error: no RTL under rtl/ in {args.ipk}", file=sys.stderr)
        return 1

    ipk_sdc = extract_ipk_constraints(args.ipk, args.output, args.top)
    ipk_docs = extract_ipk_docs(args.ipk, args.output)

    ipk_names = set(ipk_rtl) | set(ipk_sdc) | set(ipk_docs)
    supplemental = [
        name
        for name in parse_ipx_files(ipx_src)
        if name not in ipk_names and not (ipk_sdc and name.endswith(".ldc"))
    ]

    missing: list[str] = []
    for name in supplemental:
        src = find_file(name, args.top, file_dir, args.source_dir)
        if src is None:
            missing.append(name)
            continue
        dst = args.output / name
        if src.resolve() != dst.resolve():
            shutil.copy2(src, dst)

    bundled_names = list(ipk_names) + [name for name in supplemental if name not in missing]

    top_sv = f"{args.top}_top.sv"
    top_sv_src = find_top_sv(args.top, file_dir, args.source_dir)
    if top_sv_src is not None:
        top_sv_dst = args.output / top_sv
        if top_sv_src.resolve() != top_sv_dst.resolve():
            shutil.copy2(top_sv_src, top_sv_dst)
        if top_sv not in bundled_names:
            bundled_names.append(top_sv)

    references_name = f"{args.top}_references.v"
    references_path = args.output / references_name
    if references_defines_ram(references_path) and "Ram_1wrs.sv" in bundled_names:
        bundled_names.remove("Ram_1wrs.sv")
        ram_path = args.output / "Ram_1wrs.sv"
        if ram_path.is_file():
            ram_path.unlink()

    bundled_names = sorted(set(bundled_names), key=lambda n: package_file_order(n, args.top))
    package_files = [ipx_file_entry(name, args.top) for name in bundled_names]

    write_ipx(
        ipx_src,
        args.output / f"{args.top}.ipx",
        package_files,
        version=args.version,
    )

    if missing:
        print(f"Warning: missing supplemental IPX files: {', '.join(missing)}", file=sys.stderr)
        return 1

    print(f"Wrote IPX bundle to {args.output}", file=sys.stderr)
    print(f"  RTL from IPK: {', '.join(ipk_rtl)}", file=sys.stderr)
    if ipk_sdc:
        print(f"  Constraints from IPK: {', '.join(ipk_sdc)}", file=sys.stderr)
    if ipk_docs:
        print(f"  Docs from IPK: {', '.join(ipk_docs)}", file=sys.stderr)
    for path in sorted(args.output.iterdir()):
        print(f"  {path.name}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
