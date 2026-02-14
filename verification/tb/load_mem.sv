//*********************************************************************************************// 
//----------------  Load Memory -------------------------------------------------------------//// 
//---------------- Author: Xiaokun Yang  ----------------------------------------------------//// 
//---------------- Lawrence Berkeley Lab ----------------------------------------------------//// 
//---------------- Date: 6/26/2023  ---------------------------------------------------------//// 
//----- Version 1: TSQR MC Testbench---------------------------------------------------------//// 
//----- Date: 6/21/2023 ---------------------------------------------------------------------//// 
//-------------------------------------------------------------------------------------------//// 
//*********************************************************************************************// 

//---------------------------------------------------------
//--- Initialize and Load Memory -------------------------- 
//--- Single-core Test Cases ------------------------------ 
//---------------------------------------------------------
integer i;
`ifdef SINGLE_CORE
 initial begin
   `ifdef TILE_NO_2 //2 tiles
    $display("Load Memory C1R and C1D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO-0)*32-1:`MATRIX_WIDTH*(`TILE_NO-1)*32];
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO-1)*32-1:`MATRIX_WIDTH*(`TILE_NO-2)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_3 //3 tiles
    $display("Load Memory C1D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO-2)*32-1:`MATRIX_WIDTH*(`TILE_NO-3)*32];
      i=i+1;
    end
  `endif
//======================
  `ifdef TILE_NO_4 //4 tiles
    wait(mem1_fi_c_0);
    $display("Load Memory C1D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO-3)*32-1:`MATRIX_WIDTH*(`TILE_NO-4)*32];
      i=i+1;
    end
  `endif 
end
`endif //SINGLE_CORE

//---------------------------------------------------------
//--- Initialize and Load Memory -------------------------- 
//--- Dual-core Test Cases -------------------------------- 
//---------------------------------------------------------
`ifdef DUAL_CORE
initial begin
  `ifdef TILE_NO_4 //4 tiles
    $display("Load Memory C1R, C1D0, C2R, C2D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO-0)*32-1:`MATRIX_WIDTH*(`TILE_NO-1)*32];
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO-1)*32-1:`MATRIX_WIDTH*(`TILE_NO-2)*32];
      u_tsqr_mc.hh_core_1.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/2-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/2-1)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/2-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/2-2)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_6 //6 tiles
    $display("Load Memory C1D1 and C2D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO-2)*32-1:`MATRIX_WIDTH*(`TILE_NO-3)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/2-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/2-3)*32];
      i=i+1;
    end
  `endif
//======================
  `ifdef TILE_NO_8 //8 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1);
    $display("Load Memory C1D0 and C2D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO-3)*32-1:`MATRIX_WIDTH*(`TILE_NO-4)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/2-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/2-4)*32];
      i=i+1;
    end
  `endif
end
`endif //DUAL_CORE

//---------------------------------------------------------
//--- Initialize and Load Memory -------------------------- 
//--- Quad-core Test Cases -------------------------------- 
//---------------------------------------------------------
`ifdef QUAD_CORE
initial begin
  `ifdef TILE_NO_8  //8 tiles
    $display("Load Memory C1R, C1D0, C2R, C2D0, C3R, C3D0, C4R, C4D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-1)*32];
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-2)*32];
      u_tsqr_mc.hh_core_1.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-1)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-2)*32];
      u_tsqr_mc.hh_core_2.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-1)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-2)*32];
      u_tsqr_mc.hh_core_3.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-1)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-2)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_12  //12 tiles
    $display("Load Memory C1D1, C2D1, C3D1, C4D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-3)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-3)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-3)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-3)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_16 //16 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-4)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-4)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-4)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-4)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_20 //20 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-5)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-5)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-5)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-5)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_24 //24 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-6)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-6)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-6)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-6)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_28 //28 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-7)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-7)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-7)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-7)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_32 //32 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-8)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-8)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-8)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-8)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_36 //36 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-9)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-9)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-9)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-9)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_40 //40 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-10)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-10)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-10)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-10)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_44 //44 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-11)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-11)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-11)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-11)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_48 //48 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-12)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-12)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-12)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-12)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_52 //52 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-13)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-13)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-13)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-13)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_56 //56 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-14)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-14)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-14)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-14)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_60 //60 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-15)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-15)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-15)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-15)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_64 //64 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*4-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*4-16)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*3-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*3-16)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*2-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*2-16)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/4*1-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/4*1-16)*32];
      i=i+1;
    end
  `endif
end
`endif //QUAD_CORE

//---------------------------------------------------------
//--- Initialize and Load Memory -------------------------- 
//--- EIGHT-core Test Cases -------------------------------- 
//---------------------------------------------------------
`ifdef EIGHT_CORE
initial begin
  `ifdef TILE_NO_16  //16 tiles
    $display("Load Memory C1R, C1D0, C2R, C2D0, C3R, C3D0, C4R, C4D0, C5R, C5D0, C6R, C6D0, C7R, C7D0, C8R, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
        u_tsqr_mc.hh_core.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-1)*32];
        u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-2)*32];
      u_tsqr_mc.hh_core_1.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-1)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-2)*32];
      u_tsqr_mc.hh_core_2.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-1)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-2)*32];
      u_tsqr_mc.hh_core_3.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-1)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-2)*32];
      u_tsqr_mc.hh_core_4.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-1)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-2)*32];
      u_tsqr_mc.hh_core_5.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-1)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-2)*32];
      u_tsqr_mc.hh_core_6.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-1)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-2)*32];
      u_tsqr_mc.hh_core_7.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-1)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-2)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_24  //24 tiles
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-3)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-3)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-3)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-3)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-3)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-3)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-3)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-3)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_32 //32 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-4)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-4)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-4)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-4)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-4)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-4)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-4)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-4)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_40 //40 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-5)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-5)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-5)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-5)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-5)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-5)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-5)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-4)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-5)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_48 //48 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-6)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-6)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-6)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-6)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-6)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-6)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-6)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-5)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-6)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_56 //56 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-7)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-7)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-7)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-7)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-7)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-7)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-7)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-6)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-7)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_64 //64 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-8)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-8)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-8)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-8)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-8)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-8)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-8)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-7)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-8)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_72 //72 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-9)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-9)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-9)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-9)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-9)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-9)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-9)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-8)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-9)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_80 //80 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-10)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-10)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-10)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-10)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-10)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-10)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-10)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-9)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-10)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_88 //88 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-11)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-11)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-11)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-11)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-11)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-11)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-11)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-10)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-11)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_96 //96 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-12)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-12)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-12)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-12)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-12)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-12)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-12)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-11)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-12)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_104 //104 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-13)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-13)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-13)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-13)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-13)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-13)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-13)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-12)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-13)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_112 //112 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-14)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-14)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-14)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-14)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-14)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-14)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-14)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-13)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-14)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_120 //120 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-15)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-15)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-15)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-15)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-15)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-15)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-15)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-14)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-15)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_128 //128 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-16)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-16)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-16)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-16)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-16)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-16)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-16)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-15)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-16)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_136 //136 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-16)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-17)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-16)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-17)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-16)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-17)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-16)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-17)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-16)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-17)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-16)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-17)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-16)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-17)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-16)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-17)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_144 //144 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-17)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-18)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-17)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-18)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-17)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-18)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-17)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-18)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-17)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-18)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-17)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-18)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-17)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-18)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-17)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-18)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_152 //152 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-18)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-19)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-18)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-19)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-18)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-19)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-18)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-19)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-18)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-19)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-18)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-19)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-18)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-19)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-18)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-19)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_160 //160 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-19)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-20)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-19)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-20)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-19)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-20)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-19)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-20)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-19)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-20)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-19)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-20)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-19)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-20)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-19)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-20)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_168 //168 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-20)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-21)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-20)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-21)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-20)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-21)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-20)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-21)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-20)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-21)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-20)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-21)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-20)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-21)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-20)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-21)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_176 //176 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-21)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-22)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-21)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-22)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-21)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-22)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-21)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-22)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-21)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-22)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-21)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-22)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-21)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-22)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-21)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-22)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_184 //184 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-22)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-23)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-22)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-23)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-22)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-23)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-22)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-23)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-22)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-23)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-22)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-23)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-22)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-23)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-22)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-23)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_192 //192 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-23)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-24)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-23)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-24)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-23)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-24)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-23)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-24)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-23)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-24)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-23)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-24)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-23)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-24)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-23)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-24)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_200 //200 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-24)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-25)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-24)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-25)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-24)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-25)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-24)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-25)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-24)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-25)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-24)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-25)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-24)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-25)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-24)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-25)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_208 //208 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-25)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-26)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-25)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-26)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-25)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-26)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-25)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-26)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-25)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-26)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-25)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-26)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-25)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-26)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-25)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-26)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_216 //216 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-26)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-27)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-26)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-27)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-26)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-27)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-26)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-27)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-26)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-27)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-26)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-27)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-26)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-27)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-26)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-27)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_224 //224 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-27)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-28)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-27)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-28)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-27)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-28)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-27)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-28)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-27)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-28)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-27)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-28)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-27)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-28)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-27)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-28)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_232 //232 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-28)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-29)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-28)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-29)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-28)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-29)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-28)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-29)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-28)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-29)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-28)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-29)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-28)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-29)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-28)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-29)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_240 //240 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-29)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-30)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-29)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-30)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-29)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-30)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-29)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-30)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-29)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-30)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-29)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-30)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-29)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-30)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-29)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-30)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_248 //248 tiles
    wait(mem0_fi_c_0 & mem0_fi_c_1 & mem0_fi_c_2 & mem0_fi_c_3 & mem0_fi_c_4 & mem0_fi_c_5 & mem0_fi_c_6 & mem0_fi_c_7);
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-30)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-31)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-30)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-31)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-30)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-31)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-30)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-31)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-30)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-31)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-30)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-31)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-30)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-31)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-30)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-31)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_256 //256 tiles
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*8-31)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*8-32)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*7-31)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*7-32)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*6-31)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*6-32)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*5-31)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*5-32)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*4-31)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*4-32)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*3-31)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*3-32)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*2-31)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*2-32)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/8*1-31)*32-1:`MATRIX_WIDTH*(`TILE_NO/8*1-32)*32];
      i=i+1;
    end
  `endif
end
`endif //EIGHT_CORE

//---------------------------------------------------------
//--- Initialize and Load Memory -------------------------- 
//--- SIXTEEN-core Test Cases -------------------------------- 
//---------------------------------------------------------
`ifdef SIXTEEN_CORE
initial begin
  `ifdef TILE_NO_32  
    $display("Load Memory C1R, C1D0, C2R, C2D0, C3R, C3D0, C4R, C4D0, C5R, C5D0, C6R, C6D0, C7R, C7D0, C8R, C8D0 ... C16D0 C16R (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
        u_tsqr_mc.hh_core.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*16 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*16  -1)*32];
        u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*16 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*16  -2)*32];
      u_tsqr_mc.hh_core_1.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*15 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*15  -1)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*15 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*15  -2)*32];
      u_tsqr_mc.hh_core_2.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*14 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*14  -1)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*14 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*14  -2)*32];
      u_tsqr_mc.hh_core_3.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*13 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*13  -1)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*13 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*13  -2)*32];
      u_tsqr_mc.hh_core_4.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*12 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*12  -1)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*12 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*12  -2)*32];
      u_tsqr_mc.hh_core_5.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*11 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*11  -1)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*11 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*11  -2)*32];
     u_tsqr_mc.hh_core_6.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*10  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*10  -1)*32];
     u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*10  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*10  -2)*32];
      u_tsqr_mc.hh_core_7.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*9  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*9  -1)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*9  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*9  -2)*32];
      u_tsqr_mc.hh_core_8.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*8  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*8  -1)*32];
      u_tsqr_mc.hh_core_8.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*8  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*8  -2)*32];
      u_tsqr_mc.hh_core_9.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*7  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*7  -1)*32];
      u_tsqr_mc.hh_core_9.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*7  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*7  -2)*32];
     u_tsqr_mc.hh_core_10.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*6  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*6  -1)*32];
     u_tsqr_mc.hh_core_10.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*6  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*6  -2)*32];
     u_tsqr_mc.hh_core_11.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*5  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*5  -1)*32];
     u_tsqr_mc.hh_core_11.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*5  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*5  -2)*32];
     u_tsqr_mc.hh_core_12.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*4  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*4  -1)*32];
     u_tsqr_mc.hh_core_12.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*4  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*4  -2)*32];
     u_tsqr_mc.hh_core_13.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*3  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*3  -1)*32];
     u_tsqr_mc.hh_core_13.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*3  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*3  -2)*32];
     u_tsqr_mc.hh_core_14.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*2  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*2  -1)*32];
     u_tsqr_mc.hh_core_14.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*2  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*2  -2)*32];
     u_tsqr_mc.hh_core_15.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*1  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*1  -1)*32];
     u_tsqr_mc.hh_core_15.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*1  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*1  -2)*32];
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_48  
    $display("Load Memory C1D1, C2D1, C3D1, C4D1, C5D1, C6D1, C7D1, C8D1 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
        u_tsqr_mc.hh_core.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*16-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*16 -3)*32];
      u_tsqr_mc.hh_core_1.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*15-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*15 -3)*32];
      u_tsqr_mc.hh_core_2.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*14-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*14 -3)*32];
      u_tsqr_mc.hh_core_3.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*13-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*13 -3)*32];
      u_tsqr_mc.hh_core_4.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*12-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*12 -3)*32];
      u_tsqr_mc.hh_core_5.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*11-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*11 -3)*32];
      u_tsqr_mc.hh_core_6.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*10-2)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*10 -3)*32];
      u_tsqr_mc.hh_core_7.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*9-2)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*9 -3)*32];
      u_tsqr_mc.hh_core_8.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*8-2)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*8 -3)*32];
      u_tsqr_mc.hh_core_9.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*7-2)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*7 -3)*32];
     u_tsqr_mc.hh_core_10.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*6-2)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*6 -3)*32];
     u_tsqr_mc.hh_core_11.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*5-2)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*5 -3)*32];
     u_tsqr_mc.hh_core_12.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*4-2)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*4 -3)*32];
     u_tsqr_mc.hh_core_13.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*3-2)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*3 -3)*32];
     u_tsqr_mc.hh_core_14.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*2-2)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*2 -3)*32];
     u_tsqr_mc.hh_core_15.simple_dual_1.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*1-2)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*1 -3)*32];
     
     
      i=i+1;
    end
  `endif
  `ifdef TILE_NO_64 
    wait(mem1_fi_c_0 & mem1_fi_c_1 & mem1_fi_c_2 & mem1_fi_c_3 & mem1_fi_c_4 & mem1_fi_c_5 & mem1_fi_c_6 & mem1_fi_c_7);
    $display("Load Memory C1D0, C2D0, C3D0, C4D0, C5D0, C6D0, C7D0, C8D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
        u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*16-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*16 -4)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*15-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*15 -4)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*14-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*14 -4)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*13-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*13 -4)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*12-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*12 -4)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*11-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*11 -4)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*10-3)*32-1:`MATRIX_WIDTH*(`TILE_NO/16*10 -4)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*9-3)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*9 -4)*32];
      u_tsqr_mc.hh_core_8.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*8-3)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*8 -4)*32];
      u_tsqr_mc.hh_core_9.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*7-3)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*7 -4)*32];
     u_tsqr_mc.hh_core_10.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*6-3)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*6 -4)*32];
     u_tsqr_mc.hh_core_11.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*5-3)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*5 -4)*32];
     u_tsqr_mc.hh_core_12.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*4-3)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*4 -4)*32];
     u_tsqr_mc.hh_core_13.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*3-3)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*3 -4)*32];
     u_tsqr_mc.hh_core_14.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*2-3)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*2 -4)*32];
     u_tsqr_mc.hh_core_15.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/16*1-3)*32-1: `MATRIX_WIDTH*(`TILE_NO/16*1 -4)*32];
      i=i+1;
    end
  `endif
    end
  `endif
//---------------------------------------------------------
//--- Initialize and Load Memory -------------------------- 
//--- SIXTYFOUR-core Test Cases -------------------------------- 
//---------------------------------------------------------
`ifdef SIXTYFOUR_CORE
initial begin
  `ifdef TILE_NO_128  //16 tiles
    $display("Load Memory C1R, C1D0, C2R, C2D0, C3R, C3D0, C4R, C4D0, C5R, C5D0, C6R, C6D0, C7R, C7D0, C8R, C8D0 ... C64D0 C64R (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
      u_tsqr_mc.hh_core.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*64 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*64 -1)*32];
      u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*64 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*64 -2)*32];
      u_tsqr_mc.hh_core_1.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*63 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*63 -1)*32];
      u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*63 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*63 -2)*32];
      u_tsqr_mc.hh_core_2.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*62 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*62 -1)*32];
      u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*62 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*62 -2)*32];
      u_tsqr_mc.hh_core_3.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*61 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*61 -1)*32];
      u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*61 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*61 -2)*32];
      u_tsqr_mc.hh_core_4.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*60 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*60 -1)*32];
      u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*60 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*60 -2)*32];
      u_tsqr_mc.hh_core_5.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*59 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*59 -1)*32];
      u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*59 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*59 -2)*32];
      u_tsqr_mc.hh_core_6.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*58 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*58 -1)*32];
      u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*58 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*58 -2)*32];
      u_tsqr_mc.hh_core_7.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*57 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*57 -1)*32];
      u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*57 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*57 -2)*32];
      u_tsqr_mc.hh_core_8.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*56 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*56 -1)*32];
      u_tsqr_mc.hh_core_8.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*56 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*56 -2)*32];
      u_tsqr_mc.hh_core_9.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*55 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*55 -1)*32];
      u_tsqr_mc.hh_core_9.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*55 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*55 -2)*32];
      u_tsqr_mc.hh_core_10.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*54 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*54 -1)*32];
      u_tsqr_mc.hh_core_10.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*54 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*54 -2)*32];
      u_tsqr_mc.hh_core_11.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*53 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*53 -1)*32];
      u_tsqr_mc.hh_core_11.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*53 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*53 -2)*32];
      u_tsqr_mc.hh_core_12.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*52 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*52 -1)*32];
      u_tsqr_mc.hh_core_12.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*52 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*52 -2)*32];
      u_tsqr_mc.hh_core_13.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*51 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*51 -1)*32];
      u_tsqr_mc.hh_core_13.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*51 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*51 -2)*32];
      u_tsqr_mc.hh_core_14.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*50 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*50 -1)*32];
      u_tsqr_mc.hh_core_14.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*50 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*50 -2)*32];
      u_tsqr_mc.hh_core_15.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*49 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*49 -1)*32];
      u_tsqr_mc.hh_core_15.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*49 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*49 -2)*32];
      u_tsqr_mc.hh_core_16.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*48 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*48 -1)*32];
      u_tsqr_mc.hh_core_16.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*48 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*48 -2)*32];
      u_tsqr_mc.hh_core_17.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*47 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*47 -1)*32];
      u_tsqr_mc.hh_core_17.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*47 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*47 -2)*32];
      u_tsqr_mc.hh_core_18.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*46 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*46 -1)*32];
      u_tsqr_mc.hh_core_18.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*46 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*46 -2)*32];
      u_tsqr_mc.hh_core_19.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*45 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*45 -1)*32];
      u_tsqr_mc.hh_core_19.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*45 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*45 -2)*32];
      u_tsqr_mc.hh_core_20.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*44 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*44 -1)*32];
      u_tsqr_mc.hh_core_20.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*44 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*44 -2)*32];
      u_tsqr_mc.hh_core_21.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*43 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*43 -1)*32];
      u_tsqr_mc.hh_core_21.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*43 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*43 -2)*32];
      u_tsqr_mc.hh_core_22.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*42 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*42 -1)*32];
      u_tsqr_mc.hh_core_22.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*42 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*42 -2)*32];
      u_tsqr_mc.hh_core_23.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*41 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*41 -1)*32];
      u_tsqr_mc.hh_core_23.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*41 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*41 -2)*32];
      u_tsqr_mc.hh_core_24.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*40 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*40 -1)*32];
      u_tsqr_mc.hh_core_24.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*40 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*40 -2)*32];
      u_tsqr_mc.hh_core_25.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*39 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*39 -1)*32];
      u_tsqr_mc.hh_core_25.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*39 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*39 -2)*32];
      u_tsqr_mc.hh_core_26.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*38 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*38 -1)*32];
      u_tsqr_mc.hh_core_26.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*38 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*38 -2)*32];
      u_tsqr_mc.hh_core_27.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*37 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*37 -1)*32];
      u_tsqr_mc.hh_core_27.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*37 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*37 -2)*32];
      u_tsqr_mc.hh_core_28.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*36 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*36 -1)*32];
      u_tsqr_mc.hh_core_28.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*36 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*36 -2)*32];
      u_tsqr_mc.hh_core_29.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*35 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*35 -1)*32];
      u_tsqr_mc.hh_core_29.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*35 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*35 -2)*32];
      u_tsqr_mc.hh_core_30.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*34 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*34 -1)*32];
      u_tsqr_mc.hh_core_30.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*34 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*34 -2)*32];
      u_tsqr_mc.hh_core_31.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*33 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*33 -1)*32];
      u_tsqr_mc.hh_core_31.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*33 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*33 -2)*32];
      u_tsqr_mc.hh_core_32.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*32 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*32 -1)*32];
      u_tsqr_mc.hh_core_32.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*32 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*32 -2)*32];
      u_tsqr_mc.hh_core_33.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*31 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*31 -1)*32];
      u_tsqr_mc.hh_core_33.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*31 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*31 -2)*32];
      u_tsqr_mc.hh_core_34.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*30 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*30 -1)*32];
      u_tsqr_mc.hh_core_34.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*30 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*30 -2)*32];
      u_tsqr_mc.hh_core_35.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*29 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*29 -1)*32];
      u_tsqr_mc.hh_core_35.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*29 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*29 -2)*32];
     u_tsqr_mc.hh_core_36.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*28 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*28 -1)*32];
     u_tsqr_mc.hh_core_36.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*28 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*28 -2)*32];
     u_tsqr_mc.hh_core_37.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*27 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*27 -1)*32];
     u_tsqr_mc.hh_core_37.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*27 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*27 -2)*32];
     u_tsqr_mc.hh_core_38.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*26 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*26 -1)*32];
     u_tsqr_mc.hh_core_38.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*26 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*26 -2)*32];
     u_tsqr_mc.hh_core_39.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*25 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*25 -1)*32];
     u_tsqr_mc.hh_core_39.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*25 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*25 -2)*32];
     u_tsqr_mc.hh_core_40.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*24 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*24 -1)*32];
     u_tsqr_mc.hh_core_40.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*24 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*24 -2)*32];
     u_tsqr_mc.hh_core_41.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*23 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*23 -1)*32];
     u_tsqr_mc.hh_core_41.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*23 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*23 -2)*32];
     u_tsqr_mc.hh_core_42.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*22 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*22 -1)*32];
     u_tsqr_mc.hh_core_42.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*22 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*22 -2)*32];
     u_tsqr_mc.hh_core_43.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*21 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*21 -1)*32];
     u_tsqr_mc.hh_core_43.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*21 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*21 -2)*32];
     u_tsqr_mc.hh_core_44.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*20 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*20 -1)*32];
     u_tsqr_mc.hh_core_44.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*20 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*20 -2)*32];
     u_tsqr_mc.hh_core_45.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*19 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*19 -1)*32];
     u_tsqr_mc.hh_core_45.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*19 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*19 -2)*32];
     u_tsqr_mc.hh_core_46.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*18 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*18 -1)*32];
     u_tsqr_mc.hh_core_46.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*18 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*18 -2)*32];
     u_tsqr_mc.hh_core_47.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*17 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*17 -1)*32];
     u_tsqr_mc.hh_core_47.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*17 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*17 -2)*32];
     u_tsqr_mc.hh_core_48.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*16 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*16  -1)*32];
     u_tsqr_mc.hh_core_48.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*16 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*16  -2)*32];
     u_tsqr_mc.hh_core_49.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*15 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*15  -1)*32];
     u_tsqr_mc.hh_core_49.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*15 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*15  -2)*32];
     u_tsqr_mc.hh_core_50.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*14 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*14  -1)*32];
     u_tsqr_mc.hh_core_50.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*14 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*14  -2)*32];
     u_tsqr_mc.hh_core_51.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*13 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*13  -1)*32];
     u_tsqr_mc.hh_core_51.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*13 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*13  -2)*32];
     u_tsqr_mc.hh_core_52.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*12 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*12  -1)*32];
     u_tsqr_mc.hh_core_52.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*12 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*12  -2)*32];
     u_tsqr_mc.hh_core_53.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*11 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*11  -1)*32];
     u_tsqr_mc.hh_core_53.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*11 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*11  -2)*32];
     u_tsqr_mc.hh_core_54.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*10 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*10 -1)*32];
     u_tsqr_mc.hh_core_54.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*10 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*10 -2)*32];
     u_tsqr_mc.hh_core_55.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*9  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*9  -1)*32];
     u_tsqr_mc.hh_core_55.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*9  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*9  -2)*32];
     u_tsqr_mc.hh_core_56.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*8  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*8  -1)*32];
     u_tsqr_mc.hh_core_56.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*8  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*8  -2)*32];
     u_tsqr_mc.hh_core_57.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*7  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*7  -1)*32];
     u_tsqr_mc.hh_core_57.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*7  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*7  -2)*32];
     u_tsqr_mc.hh_core_58.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*6  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*6  -1)*32];
     u_tsqr_mc.hh_core_58.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*6  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*6  -2)*32];
     u_tsqr_mc.hh_core_59.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*5  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*5  -1)*32];
     u_tsqr_mc.hh_core_59.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*5  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*5  -2)*32];
     u_tsqr_mc.hh_core_60.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*4  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*4  -1)*32];
     u_tsqr_mc.hh_core_60.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*4  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*4  -2)*32];
     u_tsqr_mc.hh_core_61.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*3  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*3  -1)*32];
     u_tsqr_mc.hh_core_61.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*3  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*3  -2)*32];
     u_tsqr_mc.hh_core_62.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*2  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*2  -1)*32];
     u_tsqr_mc.hh_core_62.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*2  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*2  -2)*32];
     u_tsqr_mc.hh_core_63.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*1  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*1  -1)*32];
     u_tsqr_mc.hh_core_63.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/64*1  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/64*1  -2)*32];
      i=i+1;
    end
  `endif
    end
  `endif
//---------------------------------------------------------
//--- Initialize and Load Memory -------------------------- 
//--- 128_core Test Cases -------------------------------- 
//---------------------------------------------------------
`ifdef ONETWENTYEIGHT_CORE
initial begin
  `ifdef TILE_NO_256  //256 tiles
    $display("Load Memory C1R, C1D0, C2R, C2D0, C3R, C3D0, C4R, C4D0, C5R, C5D0, C6R, C6D0, C7R, C7D0, C8R, C8D0...C128R C128D0 (%d ns)",$time);
    for(i=0; i<`MATRIX_WIDTH;) begin
         u_tsqr_mc.hh_core.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*128-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*128-1)*32];
         u_tsqr_mc.hh_core.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*128-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*128-2)*32];
       u_tsqr_mc.hh_core_1.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*127-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*127-1)*32];
       u_tsqr_mc.hh_core_1.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*127-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*127-2)*32];
       u_tsqr_mc.hh_core_2.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*126-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*126-1)*32];
       u_tsqr_mc.hh_core_2.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*126-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*126-2)*32];
       u_tsqr_mc.hh_core_3.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*125-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*125-1)*32];
       u_tsqr_mc.hh_core_3.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*125-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*125-2)*32];
       u_tsqr_mc.hh_core_4.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*124-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*124-1)*32];
       u_tsqr_mc.hh_core_4.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*124-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*124-2)*32];
       u_tsqr_mc.hh_core_5.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*123-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*123-1)*32];
       u_tsqr_mc.hh_core_5.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*123-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*123-2)*32];
       u_tsqr_mc.hh_core_6.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*122-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*122-1)*32];
       u_tsqr_mc.hh_core_6.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*122-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*122-2)*32];
       u_tsqr_mc.hh_core_7.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*121-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*121-1)*32];
       u_tsqr_mc.hh_core_7.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*121-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*121-2)*32];
       u_tsqr_mc.hh_core_8.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*120-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*120-1)*32];
       u_tsqr_mc.hh_core_8.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*120-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*120-2)*32];
       u_tsqr_mc.hh_core_9.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*119-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*119-1)*32];
       u_tsqr_mc.hh_core_9.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*119-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*119-2)*32];
      u_tsqr_mc.hh_core_10.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*118-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*118-1)*32];
      u_tsqr_mc.hh_core_10.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*118-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*118-2)*32];
      u_tsqr_mc.hh_core_11.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*117-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*117-1)*32];
      u_tsqr_mc.hh_core_11.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*117-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*117-2)*32];
      u_tsqr_mc.hh_core_12.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*116-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*116-1)*32];
      u_tsqr_mc.hh_core_12.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*116-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*116-2)*32];
      u_tsqr_mc.hh_core_13.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*115-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*115-1)*32];
      u_tsqr_mc.hh_core_13.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*115-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*115-2)*32];
      u_tsqr_mc.hh_core_14.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*114-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*114-1)*32];
      u_tsqr_mc.hh_core_14.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*114-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*114-2)*32];
      u_tsqr_mc.hh_core_15.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*113-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*113-1)*32];
      u_tsqr_mc.hh_core_15.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*113-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*113-2)*32];
      u_tsqr_mc.hh_core_16.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*112-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*112-1)*32];
      u_tsqr_mc.hh_core_16.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*112-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*112-2)*32];
      u_tsqr_mc.hh_core_17.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*111-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*111-1)*32];
      u_tsqr_mc.hh_core_17.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*111-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*111-2)*32];
      u_tsqr_mc.hh_core_18.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*110-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*110-1)*32];
      u_tsqr_mc.hh_core_18.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*110-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*110-2)*32];
      u_tsqr_mc.hh_core_19.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*109-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*109-1)*32];
      u_tsqr_mc.hh_core_19.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*109-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*109-2)*32];
      u_tsqr_mc.hh_core_20.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*108-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*108-1)*32];
      u_tsqr_mc.hh_core_20.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*108-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*108-2)*32];
      u_tsqr_mc.hh_core_21.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*107-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*107-1)*32];
      u_tsqr_mc.hh_core_21.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*107-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*107-2)*32];
      u_tsqr_mc.hh_core_22.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*106-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*106-1)*32];
      u_tsqr_mc.hh_core_22.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*106-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*106-2)*32];
      u_tsqr_mc.hh_core_23.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*105-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*105-1)*32];
      u_tsqr_mc.hh_core_23.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*105-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*105-2)*32];
      u_tsqr_mc.hh_core_24.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*104-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*104-1)*32];
      u_tsqr_mc.hh_core_24.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*104-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*104-2)*32];
      u_tsqr_mc.hh_core_25.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*103-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*103-1)*32];
      u_tsqr_mc.hh_core_25.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*103-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*103-2)*32];
      u_tsqr_mc.hh_core_26.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*102-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*102-1)*32];
      u_tsqr_mc.hh_core_26.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*102-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*102-2)*32];
      u_tsqr_mc.hh_core_27.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*101-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*101-1)*32];
      u_tsqr_mc.hh_core_27.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*101-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*101-2)*32];
      u_tsqr_mc.hh_core_28.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*100-0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*100-1)*32];
      u_tsqr_mc.hh_core_28.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*100-1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*100-2)*32];
      u_tsqr_mc.hh_core_29.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*99 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*99 -1)*32];
      u_tsqr_mc.hh_core_29.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*99 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*99 -2)*32];
      u_tsqr_mc.hh_core_30.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*98 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*98 -1)*32];
      u_tsqr_mc.hh_core_30.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*98 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*98 -2)*32];
      u_tsqr_mc.hh_core_31.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*97 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*97 -1)*32];
      u_tsqr_mc.hh_core_31.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*97 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*97 -2)*32];
      u_tsqr_mc.hh_core_32.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*96 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*96 -1)*32];
      u_tsqr_mc.hh_core_32.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*96 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*96 -2)*32];
      u_tsqr_mc.hh_core_33.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*95 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*95 -1)*32];
      u_tsqr_mc.hh_core_33.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*95 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*95 -2)*32];
      u_tsqr_mc.hh_core_34.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*94 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*94 -1)*32];
      u_tsqr_mc.hh_core_34.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*94 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*94 -2)*32];
      u_tsqr_mc.hh_core_35.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*93 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*93 -1)*32];
      u_tsqr_mc.hh_core_35.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*93 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*93 -2)*32];
      u_tsqr_mc.hh_core_36.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*92 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*92 -1)*32];
      u_tsqr_mc.hh_core_36.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*92 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*92 -2)*32];
      u_tsqr_mc.hh_core_37.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*91 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*91 -1)*32];
      u_tsqr_mc.hh_core_37.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*91 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*91 -2)*32];
      u_tsqr_mc.hh_core_38.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*90 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*90 -1)*32];
      u_tsqr_mc.hh_core_38.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*90 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*90 -2)*32];
      u_tsqr_mc.hh_core_39.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*89 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*89 -1)*32];
      u_tsqr_mc.hh_core_39.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*89 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*89 -2)*32];
      u_tsqr_mc.hh_core_40.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*88 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*88 -1)*32];
      u_tsqr_mc.hh_core_40.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*88 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*88 -2)*32];
      u_tsqr_mc.hh_core_41.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*87 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*87 -1)*32];
      u_tsqr_mc.hh_core_41.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*87 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*87 -2)*32];
      u_tsqr_mc.hh_core_42.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*86 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*86 -1)*32];
      u_tsqr_mc.hh_core_42.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*86 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*86 -2)*32];
      u_tsqr_mc.hh_core_43.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*85 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*85 -1)*32];
      u_tsqr_mc.hh_core_43.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*85 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*85 -2)*32];
      u_tsqr_mc.hh_core_44.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*84 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*84 -1)*32];
      u_tsqr_mc.hh_core_44.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*84 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*84 -2)*32];
      u_tsqr_mc.hh_core_45.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*83 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*83 -1)*32];
      u_tsqr_mc.hh_core_45.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*83 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*83 -2)*32];
      u_tsqr_mc.hh_core_46.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*82 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*82 -1)*32];
      u_tsqr_mc.hh_core_46.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*82 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*82 -2)*32];
      u_tsqr_mc.hh_core_47.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*81 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*81 -1)*32];
      u_tsqr_mc.hh_core_47.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*81 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*81 -2)*32];
      u_tsqr_mc.hh_core_48.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*80 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*80 -1)*32];
      u_tsqr_mc.hh_core_48.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*80 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*80 -2)*32];
      u_tsqr_mc.hh_core_49.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*79 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*79 -1)*32];
      u_tsqr_mc.hh_core_49.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*79 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*79 -2)*32];
      u_tsqr_mc.hh_core_50.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*78 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*78 -1)*32];
      u_tsqr_mc.hh_core_50.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*78 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*78 -2)*32];
      u_tsqr_mc.hh_core_51.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*77 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*77 -1)*32];
      u_tsqr_mc.hh_core_51.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*77 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*77 -2)*32];
      u_tsqr_mc.hh_core_52.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*76 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*76 -1)*32];
      u_tsqr_mc.hh_core_52.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*76 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*76 -2)*32];
      u_tsqr_mc.hh_core_53.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*75 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*75 -1)*32];
      u_tsqr_mc.hh_core_53.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*75 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*75 -2)*32];
      u_tsqr_mc.hh_core_54.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*74 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*74 -1)*32];
      u_tsqr_mc.hh_core_54.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*74 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*74 -2)*32];
      u_tsqr_mc.hh_core_55.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*73 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*73 -1)*32];
      u_tsqr_mc.hh_core_55.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*73 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*73 -2)*32];
      u_tsqr_mc.hh_core_56.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*72 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*72 -1)*32];
      u_tsqr_mc.hh_core_56.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*72 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*72 -2)*32];
      u_tsqr_mc.hh_core_57.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*71 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*71 -1)*32];
      u_tsqr_mc.hh_core_57.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*71 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*71 -2)*32];
      u_tsqr_mc.hh_core_58.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*70 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*70 -1)*32];
      u_tsqr_mc.hh_core_58.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*70 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*70 -2)*32];
      u_tsqr_mc.hh_core_59.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*69 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*69 -1)*32];
      u_tsqr_mc.hh_core_59.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*69 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*69 -2)*32];
      u_tsqr_mc.hh_core_60.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*68 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*68 -1)*32];
      u_tsqr_mc.hh_core_60.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*68 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*68 -2)*32];
      u_tsqr_mc.hh_core_61.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*67 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*67 -1)*32];
      u_tsqr_mc.hh_core_61.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*67 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*67 -2)*32];
      u_tsqr_mc.hh_core_62.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*66 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*66 -1)*32];
      u_tsqr_mc.hh_core_62.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*66 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*66 -2)*32];
      u_tsqr_mc.hh_core_63.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*65 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*65 -1)*32];
      u_tsqr_mc.hh_core_63.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*65 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*65 -2)*32];
      u_tsqr_mc.hh_core_64.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*64 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*64 -1)*32];
      u_tsqr_mc.hh_core_64.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*64 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*64 -2)*32];
      u_tsqr_mc.hh_core_65.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*63 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*63 -1)*32];
      u_tsqr_mc.hh_core_65.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*63 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*63 -2)*32];
      u_tsqr_mc.hh_core_66.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*62 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*62 -1)*32];
      u_tsqr_mc.hh_core_66.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*62 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*62 -2)*32];
      u_tsqr_mc.hh_core_67.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*61 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*61 -1)*32];
      u_tsqr_mc.hh_core_67.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*61 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*61 -2)*32];
      u_tsqr_mc.hh_core_68.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*60 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*60 -1)*32];
      u_tsqr_mc.hh_core_68.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*60 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*60 -2)*32];
      u_tsqr_mc.hh_core_69.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*59 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*59 -1)*32];
      u_tsqr_mc.hh_core_69.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*59 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*59 -2)*32];
      u_tsqr_mc.hh_core_70.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*58 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*58 -1)*32];
      u_tsqr_mc.hh_core_70.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*58 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*58 -2)*32];
      u_tsqr_mc.hh_core_71.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*57 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*57 -1)*32];
      u_tsqr_mc.hh_core_71.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*57 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*57 -2)*32];
      u_tsqr_mc.hh_core_72.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*56 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*56 -1)*32];
      u_tsqr_mc.hh_core_72.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*56 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*56 -2)*32];
      u_tsqr_mc.hh_core_73.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*55 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*55 -1)*32];
      u_tsqr_mc.hh_core_73.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*55 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*55 -2)*32];
      u_tsqr_mc.hh_core_74.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*54 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*54 -1)*32];
      u_tsqr_mc.hh_core_74.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*54 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*54 -2)*32];
      u_tsqr_mc.hh_core_75.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*53 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*53 -1)*32];
      u_tsqr_mc.hh_core_75.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*53 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*53 -2)*32];
      u_tsqr_mc.hh_core_76.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*52 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*52 -1)*32];
      u_tsqr_mc.hh_core_76.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*52 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*52 -2)*32];
      u_tsqr_mc.hh_core_77.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*51 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*51 -1)*32];
      u_tsqr_mc.hh_core_77.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*51 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*51 -2)*32];
      u_tsqr_mc.hh_core_78.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*50 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*50 -1)*32];
      u_tsqr_mc.hh_core_78.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*50 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*50 -2)*32];
      u_tsqr_mc.hh_core_79.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*49 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*49 -1)*32];
      u_tsqr_mc.hh_core_79.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*49 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*49 -2)*32];
      u_tsqr_mc.hh_core_80.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*48 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*48 -1)*32];
      u_tsqr_mc.hh_core_80.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*48 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*48 -2)*32];
      u_tsqr_mc.hh_core_81.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*47 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*47 -1)*32];
      u_tsqr_mc.hh_core_81.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*47 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*47 -2)*32];
      u_tsqr_mc.hh_core_82.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*46 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*46 -1)*32];
      u_tsqr_mc.hh_core_82.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*46 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*46 -2)*32];
      u_tsqr_mc.hh_core_83.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*45 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*45 -1)*32];
      u_tsqr_mc.hh_core_83.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*45 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*45 -2)*32];
      u_tsqr_mc.hh_core_84.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*44 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*44 -1)*32];
      u_tsqr_mc.hh_core_84.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*44 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*44 -2)*32];
      u_tsqr_mc.hh_core_85.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*43 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*43 -1)*32];
      u_tsqr_mc.hh_core_85.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*43 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*43 -2)*32];
      u_tsqr_mc.hh_core_86.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*42 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*42 -1)*32];
      u_tsqr_mc.hh_core_86.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*42 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*42 -2)*32];
      u_tsqr_mc.hh_core_87.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*41 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*41 -1)*32];
      u_tsqr_mc.hh_core_87.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*41 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*41 -2)*32];
      u_tsqr_mc.hh_core_88.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*40 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*40 -1)*32];
      u_tsqr_mc.hh_core_88.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*40 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*40 -2)*32];
      u_tsqr_mc.hh_core_89.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*39 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*39 -1)*32];
      u_tsqr_mc.hh_core_89.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*39 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*39 -2)*32];
      u_tsqr_mc.hh_core_90.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*38 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*38 -1)*32];
      u_tsqr_mc.hh_core_90.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*38 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*38 -2)*32];
      u_tsqr_mc.hh_core_91.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*37 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*37 -1)*32];
      u_tsqr_mc.hh_core_91.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*37 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*37 -2)*32];
      u_tsqr_mc.hh_core_92.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*36 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*36 -1)*32];
      u_tsqr_mc.hh_core_92.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*36 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*36 -2)*32];
      u_tsqr_mc.hh_core_93.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*35 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*35 -1)*32];
      u_tsqr_mc.hh_core_93.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*35 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*35 -2)*32];
      u_tsqr_mc.hh_core_94.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*34 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*34 -1)*32];
      u_tsqr_mc.hh_core_94.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*34 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*34 -2)*32];
      u_tsqr_mc.hh_core_95.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*33 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*33 -1)*32];
      u_tsqr_mc.hh_core_95.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*33 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*33 -2)*32];
      u_tsqr_mc.hh_core_96.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*32 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*32 -1)*32];
      u_tsqr_mc.hh_core_96.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*32 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*32 -2)*32];
      u_tsqr_mc.hh_core_97.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*31 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*31 -1)*32];
      u_tsqr_mc.hh_core_97.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*31 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*31 -2)*32];
      u_tsqr_mc.hh_core_98.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*30 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*30 -1)*32];
      u_tsqr_mc.hh_core_98.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*30 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*30 -2)*32];
      u_tsqr_mc.hh_core_99.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*29 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*29 -1)*32];
      u_tsqr_mc.hh_core_99.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*29 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*29 -2)*32];
     u_tsqr_mc.hh_core_100.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*28 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*28 -1)*32];
     u_tsqr_mc.hh_core_100.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*28 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*28 -2)*32];
     u_tsqr_mc.hh_core_101.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*27 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*27 -1)*32];
     u_tsqr_mc.hh_core_101.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*27 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*27 -2)*32];
     u_tsqr_mc.hh_core_102.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*26 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*26 -1)*32];
     u_tsqr_mc.hh_core_102.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*26 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*26 -2)*32];
     u_tsqr_mc.hh_core_103.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*25 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*25 -1)*32];
     u_tsqr_mc.hh_core_103.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*25 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*25 -2)*32];
     u_tsqr_mc.hh_core_104.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*24 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*24 -1)*32];
     u_tsqr_mc.hh_core_104.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*24 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*24 -2)*32];
     u_tsqr_mc.hh_core_105.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*23 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*23 -1)*32];
     u_tsqr_mc.hh_core_105.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*23 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*23 -2)*32];
     u_tsqr_mc.hh_core_106.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*22 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*22 -1)*32];
     u_tsqr_mc.hh_core_106.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*22 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*22 -2)*32];
     u_tsqr_mc.hh_core_107.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*21 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*21 -1)*32];
     u_tsqr_mc.hh_core_107.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*21 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*21 -2)*32];
     u_tsqr_mc.hh_core_108.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*20 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*20 -1)*32];
     u_tsqr_mc.hh_core_108.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*20 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*20 -2)*32];
     u_tsqr_mc.hh_core_109.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*19 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*19 -1)*32];
     u_tsqr_mc.hh_core_109.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*19 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*19 -2)*32];
     u_tsqr_mc.hh_core_110.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*18 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*18 -1)*32];
     u_tsqr_mc.hh_core_110.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*18 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*18 -2)*32];
     u_tsqr_mc.hh_core_111.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*17 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*17 -1)*32];
     u_tsqr_mc.hh_core_111.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*17 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*17 -2)*32];
     u_tsqr_mc.hh_core_112.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*16 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*16  -1)*32];
     u_tsqr_mc.hh_core_112.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*16 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*16  -2)*32];
     u_tsqr_mc.hh_core_113.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*15 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*15  -1)*32];
     u_tsqr_mc.hh_core_113.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*15 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*15  -2)*32];
     u_tsqr_mc.hh_core_114.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*14 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*14  -1)*32];
     u_tsqr_mc.hh_core_114.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*14 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*14  -2)*32];
     u_tsqr_mc.hh_core_115.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*13 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*13  -1)*32];
     u_tsqr_mc.hh_core_115.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*13 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*13  -2)*32];
     u_tsqr_mc.hh_core_116.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*12 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*12  -1)*32];
     u_tsqr_mc.hh_core_116.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*12 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*12  -2)*32];
     u_tsqr_mc.hh_core_117.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*11 -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*11  -1)*32];
     u_tsqr_mc.hh_core_117.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*11 -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*11  -2)*32];
     u_tsqr_mc.hh_core_118.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*10  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*10  -1)*32];
     u_tsqr_mc.hh_core_118.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*10  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*10  -2)*32];
     u_tsqr_mc.hh_core_119.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*9  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*9  -1)*32];
     u_tsqr_mc.hh_core_119.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*9  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*9  -2)*32];
     u_tsqr_mc.hh_core_120.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*8  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*8  -1)*32];
     u_tsqr_mc.hh_core_120.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*8  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*8  -2)*32];
     u_tsqr_mc.hh_core_121.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*7  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*7  -1)*32];
     u_tsqr_mc.hh_core_121.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*7  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*7  -2)*32];
     u_tsqr_mc.hh_core_122.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*6  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*6  -1)*32];
     u_tsqr_mc.hh_core_122.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*6  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*6  -2)*32];
     u_tsqr_mc.hh_core_123.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*5  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*5  -1)*32];
     u_tsqr_mc.hh_core_123.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*5  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*5  -2)*32];
     u_tsqr_mc.hh_core_124.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*4  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*4  -1)*32];
     u_tsqr_mc.hh_core_124.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*4  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*4  -2)*32];
     u_tsqr_mc.hh_core_125.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*3  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*3  -1)*32];
     u_tsqr_mc.hh_core_125.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*3  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*3  -2)*32];
     u_tsqr_mc.hh_core_126.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*2  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*2  -1)*32];
     u_tsqr_mc.hh_core_126.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*2  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*2  -2)*32];
     u_tsqr_mc.hh_core_127.simple_dual_2.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*1  -0)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*1  -1)*32];
     u_tsqr_mc.hh_core_127.simple_dual.ram[i]=dmx_ram[i][`MATRIX_WIDTH*(`TILE_NO/128*1  -1)*32-1:`MATRIX_WIDTH*(`TILE_NO/128*1  -2)*32];
     i=i+1;
    end                                                                                                                     
  `endif                                                                                                                    
end                                                                                                                         
`endif //128_CORE                                                                                                           
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
