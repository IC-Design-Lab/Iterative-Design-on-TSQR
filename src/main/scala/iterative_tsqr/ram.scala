package iterative_tsqr

import chisel3._
import chisel3.util.{Cat, ShiftRegister, log2Ceil}
import Binary_Modules.BinaryDesignsNew._
import Complex_Modules._
import FP_Modules.FPUnits._


class syn_ram(bw: Int, sw: Int, depth: Int) extends Module {
  val io = IO {
    new Bundle() {
      val ena = Input(Bool())
      val enb = Input(Bool())
      val wea = Input(UInt(sw.W))
      val addra = Input(UInt((log2Ceil(depth).W)))
      val addrb = Input(UInt((log2Ceil(depth).W)))
      val dina = Input(Vec(sw, UInt(bw.W)))
      val doutb = Output(Vec(sw, UInt(bw.W)))
    }
  }
  val mem = RegInit(VecInit.fill(depth)(VecInit.fill(sw)(0.U(bw.W))))
  when(io.ena) {
    for (i <- 0 until sw) {
      when(io.wea(i)) {
        mem(io.addra)(i) := io.dina(i)
      }
    }
  }

  val data_out = RegInit(VecInit.fill(sw)(0.U(bw.W)))
  when(io.enb) {
    data_out := mem(io.addrb)
  }
  io.doutb := data_out
}