//---------------------------------------------------------------------
//------- Monitor -----------------------------------------------------
//---------------------------------------------------------------------
`define COLUMN_ROUND 36  //for st16
integer d1_index ;
integer d2_index ;
integer vk1_index;
integer d3_index ; 
integer tk_index ; 
integer d4_index ;
integer d5_index ;

reg [31:0] golden_result;
reg [31:0] dut_result   ;

reg [31:0] d1_ram [0:`MATRIX_WIDTH-1];
reg [31:0] d2_ram [0:`MATRIX_WIDTH-1];
reg [31:0] vk1_ram[0:`MATRIX_WIDTH-1];
reg [31:0] d3_ram [0:`MATRIX_WIDTH-1];
reg [31:0] tk_ram [0:`MATRIX_WIDTH-1];
reg [31:0] d4_ram [0:`COLUMN_ROUND-1];
reg [31:0] d5_ram [0:`COLUMN_ROUND-1];

initial begin
   $readmemh("../golden/single_core/sc_st16_random_test_16x8_mat/d1.txt"  , d1_ram );
   $readmemh("../golden/single_core/sc_st16_random_test_16x8_mat/d2.txt"  , d2_ram );
   $readmemh("../golden/single_core/sc_st16_random_test_16x8_mat/vk1.txt" , vk1_ram);
   $readmemh("../golden/single_core/sc_st16_random_test_16x8_mat/d3.txt"  , d3_ram );
   $readmemh("../golden/single_core/sc_st16_random_test_16x8_mat/tk.txt"  , tk_ram );
   $readmemh("../golden/single_core/sc_st16_random_test_16x8_mat/d4.txt"  , d4_ram );
   $readmemh("../golden/single_core/sc_st16_random_test_16x8_mat/d5.txt"  , d5_ram );
end

// -------------------------------------------
// --Simulation Log --------------------------
// -------------------------------------------
initial begin
  d1_index     = 0;
  d2_index     = 0;
  vk1_index    = 0;
  d3_index     = 0;
  tk_index     = 0;
  d4_index     = 0;
  d5_index     = 0;
  wait(~rst);
  forever begin
    @(negedge clk);
// -------------------------------------------
// -- HH Reflector comparison ----------------
// -------------------------------------------
    if(u_tsqr_mc.u1_hh_core.d1_vld) begin 
      golden_result = d1_ram[d1_index]       ;
      dut_result    = u_tsqr_mc.u1_hh_core.u_hh_datapath.d1;
      error_percent = (ieee754_to_fp(golden_result)-ieee754_to_fp(dut_result))/ieee754_to_fp(golden_result)*100;
        $fwrite (ref_report, "====================================%d Round Householder=============================================i\n",d1_index);
        $fwrite (ref_report, "-------------------------D1 (%d index)---------------------------------------\n",d1_index);
      if(error_percent<0) begin error_percent=-error_percent; end 
      if(error_percent<`ERROR_THRESHOLD) begin
        $fwrite (ref_report, "Comparison Pass! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (ref_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end else begin
        $fwrite (ref_report, "Comparison FAIL! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (ref_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end
      d1_index=d1_index+1;
    end

    if(u_tsqr_mc.u1_hh_core.d2_vld) begin 
      golden_result = d2_ram[d2_index]       ;
      dut_result    = u_tsqr_mc.u1_hh_core.u_hh_datapath.d2;
      error_percent = (ieee754_to_fp(golden_result)-ieee754_to_fp(dut_result))/ieee754_to_fp(golden_result)*100;
        $fwrite (ref_report, "-------------------------D2 (%d index)---------------------------------------\n",d1_index);
      if(error_percent<0) begin error_percent=-error_percent; end 
      if(error_percent<`ERROR_THRESHOLD) begin
        $fwrite (ref_report, "Comparison Pass! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (ref_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end else begin
        $fwrite (ref_report, "Comparison FAIL! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (ref_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end
      d2_index=d2_index+1;
    end

    if(u_tsqr_mc.u1_hh_core.vk1_vld) begin 
      golden_result = vk1_ram[vk1_index]       ;
      dut_result    = u_tsqr_mc.u1_hh_core.u_hh_datapath.vk1;
      error_percent = (ieee754_to_fp(golden_result)-ieee754_to_fp(dut_result))/ieee754_to_fp(golden_result)*100;
        $fwrite (ref_report, "------------------------VK1 (%d index)---------------------------------------\n",vk1_index);
      if(error_percent<0) begin error_percent=-error_percent; end 
      if(error_percent<`ERROR_THRESHOLD) begin
        $fwrite (ref_report, "Comparison Pass! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (ref_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end else begin
        $fwrite (ref_report, "Comparison FAIL! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (ref_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end
      vk1_index=vk1_index+1;
    end

    if(u_tsqr_mc.u1_hh_core.d3_vld) begin 
      golden_result = d3_ram[d3_index]       ;
      dut_result    = u_tsqr_mc.u1_hh_core.u_hh_datapath.d3;
      error_percent = (ieee754_to_fp(golden_result)-ieee754_to_fp(dut_result))/ieee754_to_fp(golden_result)*100;
        $fwrite (ref_report, "-------------------------D3 (%d index)---------------------------------------\n",d3_index);
      if(error_percent<0) begin error_percent=-error_percent; end 
      if(error_percent<`ERROR_THRESHOLD) begin
        $fwrite (ref_report, "Comparison Pass! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (ref_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end else begin
        $fwrite (ref_report, "Comparison FAIL! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (ref_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end
      d3_index=d3_index+1;
    end

    if(u_tsqr_mc.u1_hh_core.tk_vld) begin 
      golden_result = tk_ram[tk_index]       ;
      dut_result    = u_tsqr_mc.u1_hh_core.u_hh_datapath.tk;
      error_percent = (ieee754_to_fp(golden_result)-ieee754_to_fp(dut_result))/ieee754_to_fp(golden_result)*100;
        $fwrite (ref_report, "-------------------------TK (%d index)---------------------------------------\n",tk_index);
      if(error_percent<0) begin error_percent=-error_percent; end 
      if(error_percent<`ERROR_THRESHOLD) begin
        $fwrite (ref_report, "Comparison Pass! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (ref_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end else begin
        $fwrite (ref_report, "Comparison FAIL! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (ref_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end
      tk_index=tk_index+1;
    end

// -------------------------------------------
// -- HH Trailing comparison ----------------
// -------------------------------------------
    if(u_tsqr_mc.u1_hh_core.d5_rdy) begin 
      golden_result = d4_ram[d4_index]       ;
      dut_result    = u_tsqr_mc.u1_hh_core.u_hh_datapath.d4;
      error_percent = (ieee754_to_fp(golden_result)-ieee754_to_fp(dut_result))/ieee754_to_fp(golden_result)*100;
        $fwrite (trl_report, "====================================%d Round Householder=============================================i\n",vk1_index-1);
        $fwrite (trl_report, "-------------------------D4 (%d index)---------------------------------------\n",d4_index);
      if(error_percent<0) begin error_percent=-error_percent; end 
      if(error_percent<`ERROR_THRESHOLD) begin
        $fwrite (trl_report, "Comparison Pass! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (trl_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end else begin
        $fwrite (trl_report, "Comparison FAIL (%d ns)! error percent: %f%%, golden result: %f, dut result: %f\n", $time, error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (trl_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end
      d4_index=d4_index+1;
    end

    if(u_tsqr_mc.u1_hh_core.d5_vld) begin 
      golden_result = d5_ram[d5_index]       ;
      dut_result    = u_tsqr_mc.u1_hh_core.u_hh_datapath.d5;
      error_percent = (ieee754_to_fp(golden_result)-ieee754_to_fp(dut_result))/ieee754_to_fp(golden_result)*100;
        $fwrite (trl_report, "-------------------------D5 (%d index)---------------------------------------\n",d5_index);
      if(error_percent<0) begin error_percent=-error_percent; end 
      if(error_percent<`ERROR_THRESHOLD) begin
        $fwrite (trl_report, "Comparison Pass! error percent: %f%%, golden result: %f, dut result: %f\n", error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (trl_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end else begin
        $fwrite (trl_report, "Comparison FAIL! error percent (%d ns): %f%%, golden result: %f, dut result: %f\n", $time, error_percent, ieee754_to_fp(golden_result), ieee754_to_fp(dut_result));
        $fwrite (trl_report, "IEEE 754 Format! golden result: %h, dut result: %h\n", golden_result, dut_result);
      end
      d5_index=d5_index+1;
    end

  end
end
