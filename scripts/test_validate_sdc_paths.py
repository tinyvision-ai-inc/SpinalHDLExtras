#!/usr/bin/env python3
"""Fixtures for validate_sdc_paths Synplify-style get_cells matching."""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from validate_sdc_paths import (
    check_cdc_leaf_cell_globs,
    get_pins_glob_in_rtl,
    parse_modules,
    synplify_glob_hits_rtl,
    synplify_glob_matches_ident,
    token_in_rtl,
)


FLOW_V = """
module FlowCCUnsafeByToggle_1(io_input_valid, io_output_valid);
  reg [31:0] outputArea_flow_m2sPipe_payload_PRDATA;
  reg outputArea_flow_m2sPipe_valid;
  BufferCC_1 cdc_BufferCC_x (.io_dataOut(w));
endmodule
module BufferCC_1(io_dataOut);
  reg buffers_0;
  reg buffers_1;
  output io_dataOut;
endmodule
module top;
  FlowCCUnsafeByToggle_1 cdc_FlowCCUnsafeByToggle_1 ();
endmodule
"""

BAD_FLOW_SDC = """
set_false_path -through [get_nets -hierarchical {*cdc_*FlowCC*}]
set_false_path -through [get_nets -hierarchical {*cdc_BufferCC*}]
set_false_path -to [get_cells -hierarchical {*/buffers_0*}]
set_false_path -to [get_cells -hierarchical {*/buffers_1*}]
set_false_path -to [get_cells -hierarchical {*/flow_m2sPipe*}]
"""

GOOD_FLOW_SDC = """
set_false_path -through [get_nets -hierarchical {*cdc_*FlowCC*}]
set_false_path -through [get_nets -hierarchical {*cdc_BufferCC*}]
set_false_path -to [get_cells -hierarchical {*/buffers_0*}]
set_false_path -to [get_cells -hierarchical {*/buffers_1*}]
set_false_path -to [get_cells -hierarchical {*flow_m2sPipe*}]
"""


class ValidateSdcPathsTest(unittest.TestCase):
    def test_flow_m2sPipe_star_slash_does_not_match_leaf(self) -> None:
        self.assertFalse(
            synplify_glob_matches_ident(
                "*/flow_m2sPipe*", "outputArea_flow_m2sPipe_payload_PRDATA"
            )
        )
        self.assertTrue(
            synplify_glob_matches_ident(
                "*flow_m2sPipe*", "outputArea_flow_m2sPipe_payload_PRDATA"
            )
        )
        self.assertTrue(
            synplify_glob_matches_ident("*/buffers_0*", "buffers_0")
        )

    def test_token_in_rtl_uses_synplify_semantics(self) -> None:
        self.assertFalse(token_in_rtl("*/flow_m2sPipe*", FLOW_V))
        self.assertTrue(token_in_rtl("*flow_m2sPipe*", FLOW_V))
        self.assertTrue(synplify_glob_hits_rtl("*cdc_*FlowCC*", FLOW_V))

    def test_pad_port_globs_hit_header(self) -> None:
        rtl = """
module top (
  input  wire          jtag_tck,
  input  wire          jtag_tdi,
  inout  wire [3:0]    spiflash_dq,
  output wire          spiflash_clk,
  output wire          spiflash_cs_n
);
endmodule
"""
        self.assertTrue(token_in_rtl("jtag_tck*", rtl))
        self.assertTrue(token_in_rtl("spiflash_dq*", rtl))
        self.assertTrue(token_in_rtl("spiflash_cs_n*", rtl))
        # Still must not treat */flow_m2sPipe* as a hit on outputArea_flow_*
        self.assertFalse(token_in_rtl("*/flow_m2sPipe*", FLOW_V))

    def test_bad_flow_glob_reported(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            sdc = Path(td) / "t.sdc"
            v = Path(td) / "t.v"
            sdc.write_text(BAD_FLOW_SDC)
            v.write_text(FLOW_V)
            modules = parse_modules(v)
            errs = check_cdc_leaf_cell_globs("top", modules, sdc, FLOW_V)
            self.assertTrue(any("flow_m2sPipe" in e for e in errs), errs)

    def test_good_flow_glob_ok(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            sdc = Path(td) / "t.sdc"
            v = Path(td) / "t.v"
            sdc.write_text(GOOD_FLOW_SDC)
            v.write_text(FLOW_V)
            modules = parse_modules(v)
            errs = check_cdc_leaf_cell_globs("top", modules, sdc, FLOW_V)
            self.assertEqual(errs, [])

    def test_get_pins_accepts_hip_aliases(self) -> None:
        rtl = """
module top;
  wire _zz_XMAWREADY;
  wire oSCD_1_HFSDCOUT;
  wire mipi_to_bytes_clk_byte_hs_o;
  USB23_1 u (.XMAWREADY(_zz_XMAWREADY), .LMMIWDATA(w), .clk_byte_hs_o(mipi_to_bytes_clk_byte_hs_o));
  OSCD oSCD_1 (.HFSDCOUT(oSCD_1_HFSDCOUT));
  PLL pLL_1 (.REFCK(ref), .CLKOP(clkop));
endmodule
"""
        self.assertTrue(get_pins_glob_in_rtl("*/XMAWREADY", rtl))
        self.assertTrue(get_pins_glob_in_rtl("*/oSCD_1/HFSDCOUT", rtl))
        self.assertTrue(get_pins_glob_in_rtl("*/clk_byte_hs_o", rtl))
        self.assertTrue(get_pins_glob_in_rtl("*zzz_obf/pLL_1/REFCK", rtl))
        self.assertTrue(get_pins_glob_in_rtl("*/LMMIWDATA*", rtl))
        # get_cells must stay Synplify-strict (not this helper)
        self.assertFalse(token_in_rtl("*/flow_m2sPipe*", FLOW_V))


if __name__ == "__main__":
    raise SystemExit(unittest.main())
