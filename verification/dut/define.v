//*********************************************************************************************//
//---------------- Parameterized Design on TSQR----------------------------------------------////
//---------------- Author: Xiaokun Yang  ----------------------------------------------------////
//---------------- Lawrence Berkeley Lab ----------------------------------------------------////
//---------------- Date: 6/14/2023 ----------------------------------------------------------//// 
//----- Step 1: Select the TSQR core number (signle-, dual, quad-cores)----------------------////
//----- Step 2: Select the streaing width of the householder (16, 32, 64, 128, 256) ---------////
//----- Step 3: Select the MOF of FP operators (100MHz or 400 MHz for FPGA evaluation) ------////
//----- Step 4: Select the simulation or FPGA implemenation ---------------------------------////
//*********************************************************************************************//
//`define ST2_RANDOM_TEST_4X2 

// **************************************************
// -------------------------------------------------- 
// -----Parameterizable Multi-core Desgin ----------- 
// -----1. Please select the number of cores
// -------------------------------------------------- 

// -------------------------------------------------- 
// -----End Configuration
// -------------------------------------------------- 
// **************************************************


// **************************************************
// -------------------------------------------------- 
// -----Parameterizable Streaming Width of HH-------- 
// -----2. Please select the Streaming Width of HH
// -------------------------------------------------- 
`define ST_WIDTH_2

// -------------------------------------------------- 
// -----End Configuration
// -------------------------------------------------- 
// **************************************************

`ifdef ST_WIDTH_2
  `define ST_WIDTH_INF_2
`endif

`define CNT_WIDTH      16 

`ifdef ST_WIDTH_2
  `define RAM_ADDR_WIDTH 1   
  `define RAM_WIDTH      64           
  `define RAM_WEA_WIDTH  32            //128/8
  `define RAM_DEPTH      2 
  `define MATRIX_WIDTH   1  
  `define MATRIX_HEIGHT  4  
`endif

`ifdef ST_WIDTH_2
  `define WEA2    `RAM_WEA_WIDTH'hffff
  `define WEA1    `RAM_WEA_WIDTH'h00ff
`endif
  

