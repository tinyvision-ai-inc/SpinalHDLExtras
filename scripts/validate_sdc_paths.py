#!/usr/bin/env python3
"""Strict SDC path checker against Spinal Verilog instance hierarchy."""

from __future__ import annotations

import argparse
import fnmatch
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path

INST_LINE = re.compile(
    r"^\s*(?:\(\*[^*]*\*\)\s*)*"
    r"(?P<mod>[A-Za-z_][\w$]*)(?:\s*#\s*\([^;]*\))?\s+"
    r"(?P<inst>[A-Za-z_][\w$]*)\s*\("
)
MODULE_RE = re.compile(r"^\s*module\s+(\w+)")
ENDMODULE_RE = re.compile(r"^\s*endmodule\b")
GLOB_CORE_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
PATH_RE = re.compile(
    r"\[(?:get_nets|get_ports|get_pins|get_cells)\s+(?:-hierarchical\s+)?(?:\{)?([^\]\}]+?)(?:\})?\]"
)

# Leaf decls / instances Synplify get_cells can bind (module-local names).
# Allow `input wire foo` / `inout wire [3:0] bar` (direction + type).
DECL_IDENT_RE = re.compile(
    r"(?:^|\n)\s*(?:reg|wire|logic|input|output|inout)\b"
    r"(?:\s+(?:wire|reg|logic))?"
    r"(?:\s+\[[^\]]+\])?\s+"
    r"([A-Za-z_][\w$]*)",
    re.M,
)


def rtl_identifiers(text: str) -> set[str]:
    ids = set(DECL_IDENT_RE.findall(text))
    for line in text.splitlines():
        inst = INST_LINE.match(line)
        if inst:
            ids.add(inst.group("inst"))
    return ids


def synplify_glob_matches_ident(pat: str, ident: str) -> bool:
    """Approximate Synplify get_cells/get_nets -hierarchical {pat} vs a leaf name.

    ``*/flow_m2sPipe*`` requires the leaf to match ``flow_m2sPipe*`` (hierarchy
    break then prefix). It must NOT match ``outputArea_flow_m2sPipe_*``.
    """
    if fnmatch.fnmatchcase(ident, pat):
        return True
    if pat.startswith("*/"):
        return fnmatch.fnmatchcase(ident, pat[2:])
    if pat.startswith("*/") is False and "/" in pat:
        # a/b/leaf* — compare last segment only against ident
        leaf_pat = pat.rsplit("/", 1)[-1]
        return fnmatch.fnmatchcase(ident, leaf_pat)
    return False


def synplify_glob_hits_rtl(pat: str, text: str, idents: set[str] | None = None) -> bool:
    idents = idents if idents is not None else rtl_identifiers(text)
    if any(synplify_glob_matches_ident(pat, i) for i in idents):
        return True
    # Through-net globs also match hierarchical instance path strings in text.
    if "cdc_" in pat and re.search(r"\bcdc_\w+", text):
        for m in re.finditer(r"\b(cdc_[A-Za-z0-9_]+)\b", text):
            if synplify_glob_matches_ident(pat, m.group(1)):
                return True
    return False


def token_in_rtl(token: str, text: str) -> bool:
    if "*" not in token and "?" not in token:
        return token in text
    if synplify_glob_hits_rtl(token, text):
        return True
    # Pad/port globs (`jtag_tck*`, `spiflash_dq*`): hit the bare port name in
    # the module header. Does not loosen `*/flow_m2sPipe*` (core is only a
    # mid-ident substring, so \\b…\\b fails).
    cores = GLOB_CORE_RE.findall(token)
    if not cores:
        return False
    core = max(cores, key=len)
    return re.search(rf"\b{re.escape(core)}\b", text) is not None


def get_pins_glob_in_rtl(pat: str, text: str, idents: set[str] | None = None) -> bool:
    """get_pins may name HIP/blackbox pins absent as bare leaf idents.

    Soft RTL often has ``.XMAWREADY(_zz_XMAWREADY)``, ``oSCD_1_HFSDCOUT``,
    or ``.clk_byte_hs_o(...)`` rather than a cell named ``XMAWREADY``. Keep
    Synplify get_cells strictness in ``token_in_rtl``; loosen only here.
    """
    if token_in_rtl(pat, text):
        return True
    leaf = pat.rsplit("/", 1)[-1]
    cores = GLOB_CORE_RE.findall(leaf)
    if not cores:
        return False
    core = max(cores, key=len)
    if re.search(rf"\.{re.escape(core)}\s*\(", text):
        return True
    # _zz_XMAWREADY / USB23_1_XMRDATA / oSCD_1_HFSDCOUT
    if re.search(rf"[A-Za-z_][\w$]*{re.escape(core)}\b", text):
        return True
    if re.search(rf"\b{re.escape(core)}\b", text):
        return True
    idents = idents if idents is not None else rtl_identifiers(text)
    return any(core in i for i in idents)


def path_glob_in_rtl(kind: str, pat: str, text: str, idents: set[str] | None = None) -> bool:
    if kind == "get_pins":
        return get_pins_glob_in_rtl(pat, text, idents)
    return token_in_rtl(pat, text)


def hierarchical_constraint_glob_plausible(pat: str, text: str) -> bool:
    """Map/PAR or POSTSYN globs: LSR/PD are post-map pins; RTL must only expose anchors."""
    work = pat
    if work.endswith("/LSR") or work.endswith("/PD"):
        work = work.rsplit("/", 1)[0]
    literals = [p for p in re.split(r"[*?]+", work) if p]
    for lit in literals:
        lit = lit.strip("/")
        if not lit:
            continue
        for seg in lit.split("/"):
            if not seg or seg in ("LSR", "PD"):
                continue
            if seg.startswith("buffers_") or seg == "buffers":
                if "buffers_" not in text and "cdc_cap" not in text:
                    return False
                continue
            if seg not in text:
                return False
    return True


def constraint_glob_in_rtl(
    kind: str,
    pat: str,
    text: str,
    idents: set[str] | None = None,
    anchor_ok: bool = False,
    top: str | None = None,
    modules: dict[str, ModuleInfo] | None = None,
) -> bool:
    if path_glob_in_rtl(kind, pat, text, idents):
        return True
    if (
        kind == "get_cells"
        and modules is not None
        and top is not None
        and ("/" in pat or "*" in pat)
        and glob_matches_hier_cell_paths(pat, top, modules)
    ):
        return True
    if not anchor_ok:
        return False
    return hierarchical_constraint_glob_plausible(pat, text)


def check_map_pdc_policy(pdc: Path) -> list[str]:
    errors: list[str] = []
    text = pdc.read_text()
    if "u_flir_uab/" in text:
        errors.append("map PDC must use instance-agnostic globs (found u_flir_uab/)")
    if "resetCtrl_systemReset" in text and "-from" in text:
        errors.append(
            "map PDC must not use resetCtrl -from/-to pairs (use -to buffers_*/LSR|PD)"
        )
    if "ff_inst/LSR" in text or "ff_inst/PD" in text:
        errors.append("map PDC must use buffers_*/LSR|PD globs, not ff_inst/…")
    return errors


@dataclass
class ModuleInfo:
    name: str
    ports: set[str] = field(default_factory=set)
    children: dict[str, str] = field(default_factory=dict)  # inst -> child module type
    idents: set[str] = field(default_factory=set)  # module-local wires/regs/instances


def _collect_ports(header_lines: list[str]) -> set[str]:
    blob = " ".join(header_lines)
    ports: set[str] = set()
    for m in re.finditer(
        r"(?:input|output|inout)\s+(?:wire|reg)?\s*(?:\[[^\]]+\])?\s*(\w+)",
        blob,
    ):
        ports.add(m.group(1))
    paren = re.search(r"\((.*?)\)", blob, re.S)
    if paren:
        for port in paren.group(1).replace(" ", "").split(","):
            if port:
                ports.add(port)
    return ports


def parse_modules(verilog: Path) -> dict[str, ModuleInfo]:
    lines = verilog.read_text().splitlines()
    modules: dict[str, ModuleInfo] = {}
    i = 0
    while i < len(lines):
        m = MODULE_RE.match(lines[i])
        if not m:
            i += 1
            continue
        mod_name = m.group(1)
        header = [lines[i]]
        i += 1
        # module foo;  (no port list) — do not swallow following modules looking for );
        if "(" in header[0]:
            while i < len(lines) and ");" not in header[-1]:
                header.append(lines[i])
                i += 1
        info = ModuleInfo(name=mod_name, ports=_collect_ports(header))
        body_lines: list[str] = []
        while i < len(lines):
            line = lines[i]
            if ENDMODULE_RE.match(line):
                i += 1
                break
            body_lines.append(line)
            inst = INST_LINE.match(line)
            if inst:
                info.children[inst.group("inst")] = inst.group("mod")
            else:
                close_inst = re.match(r"^\s*\)\s+(\w+)\s*\(", line)
                if close_inst and info.children.get("_pending_pll_mod"):
                    info.children[close_inst.group(1)] = info.children.pop("_pending_pll_mod")
                pending = re.match(r"^\s*(PLL|OSCA|DCS)\s+#\s*\(", line)
                if pending:
                    info.children["_pending_pll_mod"] = pending.group(1)
            i += 1
        info.idents = rtl_identifiers("\n".join(body_lines))
        modules[mod_name] = info
    return modules


def resolve_path(top: str, path: str, modules: dict[str, ModuleInfo]) -> bool:
    parts = path.split("/")
    if len(parts) == 1:
        top_info = modules[top]
        return parts[0] in top_info.ports or parts[0] in top_info.children

    cur_mod = top
    for part in parts[:-1]:
        child_type = modules[cur_mod].children.get(part)
        if child_type is None:
            return False
        cur_mod = child_type

    leaf = parts[-1]
    if cur_mod not in modules:
        # Blackbox submodule (e.g. PLL): instance chain is valid.
        return True
    info = modules[cur_mod]
    if leaf in info.ports or leaf in info.children:
        return True
    # Primitive pin on a blackbox leaf instance (REFCK/CLKOP on pLL_1).
    if len(parts) >= 2 and parts[-2] in info.children:
        return True

    print(f"Could not resolve {cur_mod} {leaf} -- {info.children} {info}")
    return False


def extract_paths(sdc: Path) -> list[tuple[str, str, int]]:
    out: list[tuple[str, str, int]] = []
    for line_cnt, line in enumerate(sdc.read_text().splitlines(), 1):
        cmd = line.split("#", 1)[0].strip()
        if not cmd:
            continue
        for m in PATH_RE.finditer(cmd):
            if "get_ports" in m.group(0):
                kind = "get_ports"
            elif "get_nets" in m.group(0):
                kind = "get_nets"
            elif "get_cells" in m.group(0):
                kind = "get_cells"
            else:
                kind = "get_pins"
            raw = m.group(1).strip()
            for token in raw.split():
                token = token.strip("{}")
                if token.startswith("-"):
                    continue
                # Keep wildcards intact for Synplify-style matching.
                if token.endswith("/*"):
                    token = token[:-2]
                out.append((kind, token, line_cnt))
    return out


# Types whose instance root is TIG'd (-through). StreamFifoCC is BufferCC + RAM D only.
# Axi4CC/Apb3CC/PulseCC are containers (KeepName); nested BufferCC get cdc_*.
CDC_WALK_PREFIXES = (
    "BufferCC",
    "StreamCCByToggle",
    "FlowCCByToggle",
    "FlowCCUnsafeByToggle",
)
FIFO_CC_PREFIXES = ("StreamFifoCC",)
PULSE_CC_PREFIXES = ("PulseCCByToggle",)
CDC_CONTAINER_PREFIXES = (
    "StreamFifoCC",
    "PulseCCByToggle",
    "Axi4CC",
    "Axi4ReadOnlyCC",
    "Axi4WriteOnlyCC",
    "Apb3CC",
)


def _mod_in(mod: str, prefixes: tuple[str, ...]) -> bool:
    return any(mod == p or mod.startswith(p + "_") for p in prefixes)


def collect_instances(
    top: str, modules: dict[str, ModuleInfo], prefixes: tuple[str, ...]
) -> list[tuple[str, str]]:
    out: list[tuple[str, str]] = []

    def walk(mod: str, prefix: str) -> None:
        info = modules.get(mod)
        if info is None:
            return
        for inst, child in info.children.items():
            path = f"{prefix}/{inst}" if prefix else inst
            if _mod_in(child, prefixes):
                out.append((path, child))
            walk(child, path)

    walk(top, "")
    return out


CDC_TIMING_CELL_RE = re.compile(
    r"cdc_(cap|streamPop|flowPop|asyncFifo_spinal_port)",
    re.I,
)


def _is_cdc_timing_cell_ident(ident: str) -> bool:
    if CDC_TIMING_CELL_RE.search(ident):
        return True
    return ident.startswith("cdc_cap")


def collect_hier_cell_paths(top: str, modules: dict[str, ModuleInfo]) -> list[str]:
    """Instance/cell paths for Synplify-style get_cells -hierarchical globs."""
    out: list[str] = []

    def walk(mod: str, prefix: str) -> None:
        info = modules.get(mod)
        if info is None:
            return
        for ident in info.idents:
            if _is_cdc_timing_cell_ident(ident):
                out.append(f"{prefix}/{ident}" if prefix else ident)
        for inst, child in info.children.items():
            if inst.startswith("_"):
                continue
            path = f"{prefix}/{inst}" if prefix else inst
            walk(child, path)

    walk(top, "")
    return out


def glob_matches_hier_cell_paths(
    pat: str, top: str, modules: dict[str, ModuleInfo]
) -> bool:
    for cell_path in collect_hier_cell_paths(top, modules):
        if fnmatch.fnmatchcase(cell_path, pat):
            return True
        bare = re.sub(r"\[\d+\]", "", cell_path)
        if bare != cell_path and fnmatch.fnmatchcase(bare, pat):
            return True
    return False


def fifo_ram_glob_covers_instance(
    inst_path: str, to_pins: list[str], top: str, modules: dict[str, ModuleInfo]
) -> bool:
    cells = collect_hier_cell_paths(top, modules)
    for p in to_pins:
        if "cdc_asyncFifo_spinal_port" not in p:
            continue
        for cp in cells:
            if not cp.startswith(inst_path + "/"):
                continue
            if fnmatch.fnmatchcase(cp, p):
                return True
            bare = re.sub(r"\[\d+\]", "", cp)
            if fnmatch.fnmatchcase(bare, p):
                return True
    return False


def extract_through_net_patterns(sdc: Path) -> list[str]:
    pats: list[str] = []
    through_re = re.compile(
        r"set_false_path\s+-through\s+\[get_nets(?:\s+-hierarchical)?\s+\{([^}]+)\}\]"
    )
    for line in sdc.read_text().splitlines():
        cmd = line.split("#", 1)[0].strip()
        m = through_re.match(cmd)
        if not m:
            continue
        for token in m.group(1).split():
            pats.append(token.strip())
    return pats


def extract_to_pin_patterns(sdc: Path) -> list[str]:
    pats: list[str] = []
    to_re = re.compile(
        r"set_false_path\s+(?:-hold\s+)?-to\s+"
        r"\[(?:get_pins|get_cells)(?:\s+-hierarchical)?\s+\{([^}]+)\}\]"
    )
    for line in sdc.read_text().splitlines():
        cmd = line.split("#", 1)[0].strip()
        m = to_re.match(cmd)
        if not m:
            continue
        for token in m.group(1).split():
            pats.append(token.strip())
    return pats


def pin_covers(probe: str, patterns: list[str]) -> bool:
    return any(fnmatch.fnmatchcase(probe, pat) for pat in patterns)


def through_covers(inst_path: str, patterns: list[str]) -> bool:
    probe = inst_path + "/x"
    for pat in patterns:
        if "*" in pat:
            if fnmatch.fnmatchcase(probe, pat) or fnmatch.fnmatchcase(inst_path, pat):
                return True
        elif inst_path == pat or inst_path == pat.rstrip("/*"):
            return True
        elif pat.endswith("/*") and inst_path == pat[:-2]:
            return True
    return False


# Post-map get_nets never carry these type strings (Synplify MT447).
DEAD_CDC_TYPE_TOKENS = (
    "StreamCCByToggle",
    "FlowCCByToggle",
    "FlowCCUnsafeByToggle",
)


def check_dead_through_globs(sdc: Path) -> list[str]:
    errors: list[str] = []
    for pat in extract_through_net_patterns(sdc):
        for tok in DEAD_CDC_TYPE_TOKENS:
            if tok in pat:
                errors.append(
                    f"dead through glob {pat} (post-map nets lose {tok}; "
                    "use *cdc_BufferCC* / *cdc_*ccToggle* / *cdc_*FlowCC*)"
                )
                break
    return errors


USB23_HOLD_LEAVES = (
    "XMAWREADY",
    "XMWREADY",
    "XMBID",
    "XMBVALID",
    "XMBRESP",
    "XMARREADY",
    "XMRID",
    "XMRVALID",
    "XMRLAST",
    "XMRDATA",
    "LMMIWDATA",
)
USB23_BAD_LEAVES = ("LMMIDATA",)


def check_usb23_hold_pins(verilog_text: str, sdc: Path) -> list[str]:
    pats = extract_to_pin_patterns(sdc)
    if not any(any(leaf in p for leaf in USB23_HOLD_LEAVES + USB23_BAD_LEAVES) for p in pats):
        return []
    errors: list[str] = []
    for bad in USB23_BAD_LEAVES:
        if any(bad in p for p in pats):
            errors.append(
                f"USB23 hold glob uses {bad}* (use LMMIWDATA*; "
                f"{bad} does not match LMMIWDATA)"
            )
    # Require pin tokens exist in plaintext RTL when USB holds are present.
    if "pragma protect" in verilog_text:
        return errors
    for leaf in USB23_HOLD_LEAVES:
        if leaf not in verilog_text:
            errors.append(f"USB23 hold leaf {leaf} not found in Verilog")
        elif not any(leaf in p for p in pats):
            errors.append(f"missing USB23 hold false_path for {leaf}")
    return errors


def check_cdc_leaf_cell_globs(
    top: str, modules: dict[str, ModuleInfo], sdc: Path, verilog_text: str
) -> list[str]:
    """Ensure get_cells CDC globs hit real RTL leaves (Synplify semantics)."""
    to_pins = extract_to_pin_patterns(sdc)
    idents = rtl_identifiers(verilog_text)
    errors: list[str] = []

    def hits(substr: str) -> bool:
        return any(substr in i for i in idents) or substr in verilog_text

    def covered(substr: str) -> bool:
        for p in to_pins:
            if substr not in p:
                continue
            if synplify_glob_hits_rtl(p, verilog_text, idents):
                return True
            if glob_matches_hier_cell_paths(p, top, modules):
                return True
        return False

    stream_cc = collect_instances(top, modules, ("StreamCCByToggle",))
    if stream_cc and hits("cdc_streamPop"):
        if not covered("cdc_streamPop"):
            errors.append("missing get_cells TIG for cdc_streamPop*")
    elif stream_cc and hits("popArea_stream_rData"):
        if not any("popArea_stream_rData" in p for p in to_pins):
            errors.append("missing get_cells TIG for popArea_stream_rData*")
        elif not covered("popArea_stream_rData"):
            errors.append(
                "popArea_stream_rData get_cells glob does not match RTL leaves"
            )

    flow_cc = collect_instances(
        top, modules, ("FlowCCByToggle", "FlowCCUnsafeByToggle")
    )
    if flow_cc and hits("cdc_flowPop"):
        if not covered("cdc_flowPop"):
            errors.append("missing get_cells TIG for cdc_flowPop*")
    elif flow_cc and hits("flow_m2sPipe"):
        if not any("flow_m2sPipe" in p for p in to_pins):
            errors.append("missing get_cells TIG for flow_m2sPipe*")
        elif not covered("flow_m2sPipe"):
            errors.append(
                "flow_m2sPipe get_cells glob does not match RTL leaves "
                "(use *flow_m2sPipe*, not */flow_m2sPipe* — leaf is "
                "outputArea_flow_m2sPipe_*)"
            )
    # Catch the known bad form even if another glob also matches.
    for p in to_pins:
        if p.startswith("*/flow_m2sPipe"):
            errors.append(
                f"bad FlowCC cell glob {p}: Synplify */prefix requires "
                "leaf to start with flow_m2sPipe (actual: "
                "outputArea_flow_m2sPipe_*); use *flow_m2sPipe*"
            )

    if collect_instances(top, modules, ("BufferCC",)) and hits("cdc_cap"):
        if not covered("cdc_cap"):
            errors.append("missing get_cells TIG for cdc_cap*")
    elif collect_instances(top, modules, ("BufferCC",)) and hits("buffers_0"):
        if not any("buffers_0" in p for p in to_pins):
            errors.append("missing get_cells TIG for buffers_0*")
        elif not covered("buffers_0"):
            errors.append("buffers_0 get_cells glob does not match RTL leaves")

    return errors


def check_cdc_false_paths(
    top: str, modules: dict[str, ModuleInfo], sdc: Path
) -> list[str]:
    patterns = extract_through_net_patterns(sdc)
    to_pins = extract_to_pin_patterns(sdc)
    errors: list[str] = []
    def cell_glob_covers(path: str, mod: str) -> bool:
        # Capture-flop globs. One pattern covers every renamed instance.
        if mod.startswith("BufferCC"):
            return any(
                "cdc_cap" in p
                and "cdc_BufferCC" in p
                and glob_matches_hier_cell_paths(p, top, modules)
                for p in to_pins
            )
        if mod.startswith("StreamCC"):
            return any("cdc_streamPop" in p for p in to_pins)
        if "FlowCC" in mod:
            return any(
                "cdc_flowPop" in p
                and glob_matches_hier_cell_paths(p, top, modules)
                for p in to_pins
            )
        return False

    cdc = collect_instances(top, modules, CDC_WALK_PREFIXES)
    for path, mod in cdc:
        if through_covers(path, patterns) or cell_glob_covers(path, mod):
            continue
        errors.append(f"missing false path for {mod} {path}")
    for path, mod in collect_instances(top, modules, FIFO_CC_PREFIXES):
        if through_covers(path, patterns):
            errors.append(
                f"whole StreamFifoCC TIG (use nested BufferCC *cdc_* and "
                f"get_cells *cdc_asyncFifo_spinal_port*) {mod} {path}"
            )
    fifo_inst = collect_instances(top, modules, FIFO_CC_PREFIXES)
    if fifo_inst:
        has_async = any("cdc_asyncFifo_spinal_port" in p for p in to_pins)
        has_ram = any("ram_spinal_port1" in p for p in to_pins)
        if not has_async and not has_ram:
            errors.append(
                "missing StreamFifoCC RAM false_path "
                "cdc_asyncFifo_spinal_port*"
            )
        elif has_async:
            for inst_path, mod in fifo_inst:
                if not fifo_ram_glob_covers_instance(
                    inst_path, to_pins, top, modules
                ):
                    errors.append(
                        f"no FIFO RAM get_cells glob covers instance "
                        f"{inst_path} ({mod})"
                    )
        elif has_ram and not has_async:
            if any(
                p.startswith("*ram_spinal") and not p.startswith("*/ram_spinal")
                for p in to_pins
            ):
                errors.append(
                    "StreamFifoCC RAM TIG must be */ram_spinal_port1* "
                    "(bare *ram_spinal_port1* also hits same-clock logic_ram_*)"
                )
            elif not any(
                "*/ram_spinal_port1" in p or p.endswith("ram_spinal_port1*")
                for p in to_pins
                if "ram_spinal_port1" in p
            ):
                errors.append(
                    "StreamFifoCC RAM TIG must be get_cells */ram_spinal_port1* "
                    "(get_pins …ff_inst/DF is Synplify-escaped to \\.ff_inst)"
                )
    for path, mod in collect_instances(top, modules, PULSE_CC_PREFIXES):
        if through_covers(path, patterns):
            errors.append(f"PulseCCByToggle parent TIG (same-clock toggle) {mod} {path}")
    cdc_paths = {p for p, _ in cdc}
    pulse_paths = {p for p, _ in collect_instances(top, modules, PULSE_CC_PREFIXES)}
    fifo_paths = {p for p, _ in collect_instances(top, modules, FIFO_CC_PREFIXES)}
    container_paths = {
        p for p, _ in collect_instances(top, modules, CDC_CONTAINER_PREFIXES)
    }

    def under(path: str, roots: set[str]) -> bool:
        return path in roots or any(path.startswith(r + "/") for r in roots)

    def all_inst(mod: str, prefix: str) -> list[tuple[str, str]]:
        acc: list[tuple[str, str]] = []
        info = modules.get(mod)
        if info is None:
            return acc
        for inst, child in info.children.items():
            path = f"{prefix}/{inst}" if prefix else inst
            acc.append((path, child))
            acc.extend(all_inst(child, path))
        return acc

    glob_pats = [p for p in patterns if "*" in p]
    for path, mod in all_inst(top, ""):
        if (
            under(path, cdc_paths)
            or under(path, pulse_paths)
            or under(path, fifo_paths)
            or under(path, container_paths)
            or path in container_paths
        ):
            continue
        hits = [g for g in glob_pats if fnmatch.fnmatchcase(path + "/x", g)]
        if hits:
            errors.append(f"glob TIG on non-CDC {mod} {path} via {hits}")
    return errors


def validate_constraint_file(
    top: str,
    modules: dict[str, ModuleInfo],
    verilog_text: str,
    idents: set[str],
    constraint: Path,
    *,
    anchor_globs: bool,
    run_cdc: bool,
    label: str,
) -> tuple[int, list[str]]:
    """Return (exit_code, policy_errors). exit_code 0 = paths ok."""
    missing: list[tuple[str, str, int]] = []
    glob_miss: list[tuple[str, str, int]] = []
    for kind, path, lineno in extract_paths(constraint):
        if path == "clk":
            continue
        if "*" in path or "?" in path:
            if not constraint_glob_in_rtl(
                kind,
                path,
                verilog_text,
                idents,
                anchor_ok=anchor_globs,
                top=top,
                modules=modules,
            ):
                glob_miss.append((kind, path, lineno))
            continue
        if not resolve_path(top, path, modules):
            missing.append((kind, path, lineno))

    if missing or glob_miss:
        print(
            f"Unresolved paths in {top} from {constraint.name}{label}:",
            file=sys.stderr,
        )
        for kind, path, lineno in sorted(set(missing)):
            print(f"  [{kind}] {path} ({constraint}:{lineno})", file=sys.stderr)
        for kind, path, lineno in sorted(set(glob_miss)):
            print(f"  [{kind}] glob {path} ({constraint}:{lineno})", file=sys.stderr)
        return 1, []

    if not run_cdc:
        return 0, []

    cdc_errs = check_cdc_false_paths(top, modules, constraint)
    leaf_errs = check_cdc_leaf_cell_globs(top, modules, constraint, verilog_text)
    dead_errs = check_dead_through_globs(constraint)
    usb_errs = check_usb23_hold_pins(verilog_text, constraint)
    return 0, cdc_errs + leaf_errs + dead_errs + usb_errs


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--top", required=True)
    ap.add_argument("--verilog", type=Path, required=True)
    ap.add_argument("--sdc", type=Path, required=True)
    ap.add_argument(
        "--map-pdc",
        type=Path,
        default=None,
        help="Optional Map/PAR .pdc (anchor globs + policy; no CDC coverage)",
    )
    ap.add_argument(
        "--label",
        default="",
        help="Optional stage label (pre-yosys / post-yosys) for error messages",
    )
    args = ap.parse_args()
    label = f" ({args.label})" if args.label else ""

    modules = parse_modules(args.verilog)
    if args.top not in modules:
        print(f"Top module {args.top} not found{label}", file=sys.stderr)
        return 2

    verilog_text = args.verilog.read_text()
    idents = rtl_identifiers(verilog_text)

    path_rc, cdc_errs = validate_constraint_file(
        args.top,
        modules,
        verilog_text,
        idents,
        args.sdc,
        anchor_globs=False,
        run_cdc=True,
        label=label,
    )
    if path_rc != 0:
        return path_rc

    if args.map_pdc is not None:
        policy = check_map_pdc_policy(args.map_pdc)
        if policy:
            print(f"Map PDC policy errors in {args.top}{label}:", file=sys.stderr)
            for e in policy:
                print(f"  {e}", file=sys.stderr)
            return 1
        map_rc, _ = validate_constraint_file(
            args.top,
            modules,
            verilog_text,
            idents,
            args.map_pdc,
            anchor_globs=True,
            run_cdc=False,
            label=label,
        )
        if map_rc != 0:
            return map_rc

    if cdc_errs:
        print(
            f"CDC/USB false-path coverage errors in {args.top}{label}:",
            file=sys.stderr,
        )
        for e in cdc_errs:
            print(f"  {e}", file=sys.stderr)
        return 1

    n_paths = len(extract_paths(args.sdc))
    if args.map_pdc is not None:
        n_paths += len(extract_paths(args.map_pdc))
    print(f"All {n_paths} constraint references resolve in {args.top}{label}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
