#!/usr/bin/env python3
"""rewrite_sdc_for_yosys must keep Radiant {glob} braces with -hierarchical."""

from __future__ import annotations

import unittest

from rewrite_sdc_for_yosys import rewrite_line
from validate_sdc_paths import ModuleInfo


class RewriteSdcTest(unittest.TestCase):
    def test_hierarchical_brace_globs_preserved(self) -> None:
        v = "module top;\nendmodule\n"
        modules = {"top": ModuleInfo(name="top", ports=set(), children={})}
        lines = [
            "set_false_path -through [get_nets -hierarchical {*cdc_BufferCC*}]",
            "set_false_path -through [get_nets -hierarchical {*cdc_*FlowCC*}]",
            "set_false_path -hold -to [get_pins -hierarchical {*/XMAWREADY}]",
            "set_false_path -to [get_cells -hierarchical {*flow_m2sPipe*}]",
            "create_generated_clock -name {CLKOP} -source "
            "[get_pins -hierarchical {*zzz_1/pLL_1/REFCK}] "
            "-multiply_by 5 -divide_by 3 "
            "[get_pins -hierarchical {*zzz_1/pLL_1/CLKOP}]",
        ]
        for line in lines:
            out = rewrite_line(line, "top", modules, v)
            self.assertIn("{", out, msg=out)
            self.assertNotRegex(out, r"-hierarchical \*", msg=out)
            self.assertEqual(out.count("{"), out.count("}"))


if __name__ == "__main__":
    raise SystemExit(unittest.main())
