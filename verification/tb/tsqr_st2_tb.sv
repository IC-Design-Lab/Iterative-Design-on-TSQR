//*********************************************************************************************// 
//----------------   TSQR Testbench (Scala golden model)-------------------------------------//// 
//---------------- Author: Xiaokun Yang  ----------------------------------------------------//// 
//---------------- Lawrence Berkeley Lab ----------------------------------------------------//// 
//---------------- Date: 6/13/2023  ---------------------------------------------------------//// 
//----- Version 1: TSQR MC Testbench---------------------------------------------------------//// 
//----- Date: 6/13/2023 ---------------------------------------------------------------------//// 
//-------------------------------------------------------------------------------------------//// 
//*********************************************************************************************// 
//----- This testbench support verification to the multi-core TSQR designs                     //
//----- with streaming width of 16                                                             // 
//*********************************************************************************************// 
`define ERROR_THRESHOLD 5  //5% error tolerace
`define BW 64
//--------------------------------------------------------- 
//--- test cases  
//--------------------------------------------------------- 
`ifdef ST2_RANDOM_TEST_4X2
  `define TILE_NO 2
`endif


module tsqr_st2_tb();
//---------------------------------------------------------
//--- wire and reg declaration 
//---------------------------------------------------------
reg                         clock        ;
reg                         reset        ;
reg                         io_tsqr_en    ;
reg  [32 - 1:0]     io_mx_no    ;
reg                         io_r_vld;

wire                        io_mem0_fi ;
wire                        io_mem1_fi ;

wire                        io_tsqr_fi    ; 
reg [`RAM_WIDTH-1:0]          new_ug_i;
reg [`BW-1:0]                 io_r_0;
//---------------------------------------------------------
//--- golden model and input file 
//---------------------------------------------------------


initial begin
$display("=== The maxix factoriation Starts! (%d ns) ====", $time);

  `ifdef ST2_RANDOM_TEST_4X2
     $readmemh("../golden/sc_st2_random_test_4x2/r_buffer.txt" , u_tsqr_topmodule.tri_.mem_0_ext.Memory);
     $readmemh("../golden/sc_st2_random_test_4x2/m0_buffer.txt" , u_tsqr_topmodule.dm0.mem_0_ext.Memory);


`endif
end

//---------------------------------------------------------
//--- Initialize and Load Memory -------------------------- 
//--- Single-core Test Cases ------------------------------ 
//---------------------------------------------------------
//`include "load_mem.sv"

//---------------------------------------------------------
//--- Instantiation 
//---------------------------------------------------------
tsqr_topmodule u_tsqr_topmodule (.clock        (clock          ),
                   .reset            (reset          ),
	           .io_tsqr_en        (io_tsqr_en      ),
		   .io_r_vld             (io_r_vld),
		   .io_tsqr_fi              (io_tsqr_fi),
		   .io_mem0_fi             (io_mem0_fi),
		   .io_mem1_fi          (io_mem1_fi),
		   .io_mx_no          (io_mx_no),
		   .io_r_0       (io_r_0));

//---------------------------------------------------------------------
//------- BFM
//---------------------------------------------------------------------
integer i, j;
always #5 clock = ~clock;
initial begin
   reset           = 1'b1;
   clock          = 1'b0;
   io_tsqr_en     = 1'b0;
   io_mx_no       = `TILE_NO;
  
   #100;
   reset     = 1'b0;
  #16;
  io_tsqr_en = 1'b1;


   

end


//---------------------------------------------------------------------
//------- Monitor -----------------------------------------------------
//---------------------------------------------------------------------

endmodule
