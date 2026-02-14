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

  val mem = SyncReadMem(depth, Vec(sw, UInt(bw.W)))

  when(io.ena) {
    val data_in = Wire(Vec(sw, UInt(bw.W)))
    data_in := io.dina
    mem.write(io.addra, data_in, io.wea.asBools)
  }

  val data_out = mem.read(io.addrb, io.enb)
  io.doutb := data_out
}