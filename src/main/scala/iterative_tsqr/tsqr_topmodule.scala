package iterative_tsqr

import chisel3._
import chisel3.util.{Cat, ShiftRegister, log2Ceil}
import Binary_Modules.BinaryDesignsNew._
import Complex_Modules._
import FP_Modules.FPUnits._


class tsqr_topmodule (bw: Int, sw_ram: Int, sw_loops: Int, k: Int, c: Int, mult_pd: Int, add_pd: Int, div_pd: Int, sqrt_pd: Int, depth: Int) extends Module {

  val io = IO (new Bundle {
    val tsqr_en = Input(Bool())
    val r_vld = Output(Bool())
    val tsqr_fi = Output(Bool())
    val mem0_fi = Output(Bool())
    val mem1_fi = Output(Bool())
    val mx_no = Input(UInt(32.W))
    val r = Output(Vec(sw_ram, UInt(bw.W)))

  })


  io.r_vld := false.B
  io.tsqr_fi := false.B
  io.mem0_fi := false.B
  io.mem1_fi := false.B
  io.r := VecInit.fill(sw_ram)(0.U(bw.W))


  val tri = Module(new syn_ram(bw, sw_ram, depth))
  val dm0 = Module(new syn_ram(bw, sw_ram, depth))
  val dm1 = Module(new syn_ram(bw, sw_ram, depth))

  val tri_ena = WireDefault(false.B)
  val tri_wea = WireDefault(0.U(sw_ram.W))
  val tri_addra = WireDefault(0.U((log2Ceil(depth).W)))
  val tri_dina = WireDefault(VecInit.fill(sw_ram)(0.U(bw.W)))
  val tri_enb = WireDefault(false.B)
  val tri_addrb = WireDefault(0.U((log2Ceil(depth).W)))

  val dm0_ena = WireDefault(false.B)
  val dm0_wea = WireDefault(0.U(sw_ram.W))
  val dm0_addra = WireDefault(0.U((log2Ceil(depth).W)))
  val dm0_dina = WireDefault(VecInit.fill(sw_ram)(0.U(bw.W)))
  val dm0_enb = WireDefault(false.B)
  val dm0_addrb = WireDefault(0.U((log2Ceil(depth).W)))



  val dm1_ena = WireDefault(false.B)
  val dm1_wea = WireDefault(0.U(sw_ram.W))
  val dm1_addra = WireDefault(0.U((log2Ceil(depth).W)))
  val dm1_dina = WireDefault(VecInit.fill(sw_ram)(0.U(bw.W)))
  val dm1_enb = WireDefault(false.B)
  val dm1_addrb = WireDefault(0.U((log2Ceil(depth).W)))


  tri.io.ena := tri_ena
  tri.io.wea := tri_wea
  tri.io.addra := tri_addra
  tri.io.dina := tri_dina
  tri.io.enb := tri_enb
  tri.io.addrb := tri_addrb
  val tri_doutb = tri.io.doutb

  dm0.io.ena := dm0_ena
  dm0.io.wea := dm0_wea
  dm0.io.addra := dm0_addra
  dm0.io.dina := dm0_dina
  dm0.io.enb := dm0_enb
  dm0.io.addrb := dm0_addrb
  val dm0_doutb = dm0.io.doutb

  dm1.io.ena := dm1_ena
  dm1.io.wea := dm1_wea
  dm1.io.addra := dm1_addra
  dm1.io.dina := dm1_dina
  dm1.io.enb := dm1_enb
  dm1.io.addrb := dm1_addrb
  val dm1_doutb = dm1.io.doutb

  val outerloop = Module(new tsqr_outer_loop(bw, sw_loops, k, mult_pd, add_pd, div_pd, sqrt_pd))
  val innerloop = Module(new tsqr_inner_loop(bw, sw_loops, k, c, mult_pd, add_pd))

  outerloop.io.xk_in := VecInit.fill(sw_loops)(0.U(bw.W))
  outerloop.io.in_valid := false.B
  outerloop.io.counter_reset := false.B
  outerloop.io.in_en := false.B

  innerloop.io.xk_in := VecInit.fill(sw_loops)(0.U(bw.W))
  innerloop.io.alpha_in := 0.U
  innerloop.io.tk_in := 0.U
  innerloop.io.column_count := 0.U
  innerloop.io.en_in := false.B
  innerloop.io.valid_in := false.B
  innerloop.io.counter_reset := false.B

  val columns_left = RegInit(c.U(32.W))
  val tk_done_reg = RegInit(false.B)
  val innerloop_enable = RegInit(false.B)
  val c2_done_reg = RegInit(false.B)
  val first_start = RegInit(true.B)
  val tsqr_finished = RegInit(false.B)
  val runs_left = RegInit(io.mx_no)

  when (tsqr_finished) {
    io.tsqr_fi := true.B
    io.r_vld := true.B
  }

  innerloop.io.column_count := columns_left

  val counter1 = RegInit(0.U(32.W))
  val counter2 = RegInit(0.U(32.W))
  val counter3 = RegInit(0.U(32.W))
  val counter4 = RegInit(0.U(32.W))

  when (io.tsqr_en && !tsqr_finished) {

    when (first_start && !innerloop_enable) {

      when (first_start) {
        counter1 := counter1 + 1.U
      }

      when (counter1 >= 1.U) {
        outerloop.io.in_valid := true.B
        outerloop.io.in_en := true.B
      }

      outerloop.io.xk_in(0) := tri_doutb(0)
      outerloop.io.xk_in(1) := dm0_doutb(0)

      when (counter1 <= 1.U) {
        tri_enb := true.B
        dm0_enb := true.B
        tri_addrb := counter1
        dm0_addrb := counter1
      }.otherwise {
        tri_enb := false.B
        dm0_enb := false.B
      }
    }

    when (outerloop.io.out_valid) {
      tk_done_reg := true.B
    }

    when (tk_done_reg) {
      innerloop_enable := true.B
      c2_done_reg := false.B
      first_start := false.B
    }

    when (innerloop_enable) {

      counter2 := counter2 + 1.U

      when (counter2 === 0.U) {
        c2_done_reg := false.B
        counter3 := 0.U
      }

      tri_enb := false.B
      dm0_enb := false.B

      when (columns_left === c.U) {
        when (counter2 <= 3.U) {
          tri_enb   := true.B
          dm0_enb   := true.B
          tri_addrb := counter2
          dm0_addrb := counter2
        }
      }.elsewhen (columns_left === (c.U - 1.U)) {

        when (counter2 === 0.U) {
          tri_enb   := true.B
          dm0_enb   := true.B
          tri_addrb := 3.U
          dm0_addrb := 2.U
        }.elsewhen (counter2 === 1.U) {
          tri_enb   := false.B
          dm0_enb   := true.B
          dm0_addrb := 3.U
        }
      }

      val read_valid = RegNext(tri_enb || dm0_enb)
      val read_counter = RegNext(counter2)

      innerloop.io.alpha_in := outerloop.io.alpha_out
      innerloop.io.tk_in    := outerloop.io.out_tk

      when (counter2 >= 1.U) {
        innerloop.io.valid_in := true.B
        innerloop.io.en_in := true.B
      }

      innerloop.io.xk_in(0) := 0.U
      innerloop.io.xk_in(1) := 0.U

      when (read_valid) {
        when(columns_left === c.U) {
          innerloop.io.xk_in(0) := tri_doutb(0)
          innerloop.io.xk_in(1) := dm0_doutb(0)
        }.elsewhen(columns_left === (c.U - 1.U)) {
          when(read_counter === 0.U) {
            innerloop.io.xk_in(0) := tri_doutb(0)
            innerloop.io.xk_in(1) := dm0_doutb(0)
          }.elsewhen(read_counter === 1.U) {
            innerloop.io.xk_in(0) := dm0_doutb(0)
            innerloop.io.xk_in(1) := 0.U
          }
        }
      }

      when (columns_left === c.U) {
        when(read_counter > 3.U) {
          innerloop.io.xk_in(0) := 0.U
          innerloop.io.xk_in(1) := 0.U
        }
      }.elsewhen(columns_left === c.U-1.U) {
        when(read_counter > 1.U) {
          innerloop.io.xk_in(0) := 0.U
          innerloop.io.xk_in(1) := 0.U
        }
      }

    }

    when (innerloop.io.updates_done) {
      columns_left := columns_left - 1.U
      runs_left := runs_left - 1.U
      innerloop.io.counter_reset := true.B
      innerloop_enable := false.B
      counter2 := 0.U
      tk_done_reg := false.B
      counter4 := 0.U
      io.mem0_fi := true.B

      when (columns_left === 1.U) {
        tsqr_finished := true.B

      }
    }

    when (innerloop.io.col2_done) {
      outerloop.io.counter_reset := true.B
      c2_done_reg := true.B
      counter3 := 0.U
      first_start := false.B
    }

    when(!innerloop.io.valid_out) {
      counter4 := 0.U
    }
    when (innerloop.io.valid_out) {

      counter4 := counter4 + 1.U

      when(columns_left === c.U) {
        when(counter4 === 0.U) {
          tri_ena := true.B
          dm0_ena := true.B
          tri_wea := 1.U
          dm0_wea := 1.U
          tri_addra := 0.U
          dm0_addra := 0.U
          tri_dina(0) := innerloop.io.out_s(0)
          dm0_dina(0) := innerloop.io.out_s(1)
        }.elsewhen(counter4 === 1.U) {
          tri_ena := true.B
          dm0_ena := true.B
          tri_wea := 1.U
          dm0_wea := 1.U
          tri_addra := 1.U
          dm0_addra := 1.U
          tri_dina(0) := innerloop.io.out_s(0)
          dm0_dina(0) := innerloop.io.out_s(1)
        }.elsewhen(counter4 === 2.U) {
          tri_ena := true.B
          dm0_ena := true.B
          tri_wea := 1.U
          dm0_wea := 1.U
          tri_addra := 2.U
          dm0_addra := 2.U
          tri_dina(0) := innerloop.io.out_s(0)
          dm0_dina(0) := innerloop.io.out_s(1)
        }.elsewhen(counter4 === 3.U) {
          tri_ena := true.B
          dm0_ena := true.B
          tri_wea := 1.U
          dm0_wea := 1.U
          tri_addra := 3.U
          dm0_addra := 3.U
          tri_dina(0) := innerloop.io.out_s(0)
          dm0_dina(0) := innerloop.io.out_s(1)
        }.otherwise{
          tri_ena := false.B
          dm0_ena := false.B
        }
      }.elsewhen(columns_left === (c.U - 1.U)) {
        when(counter4 === 0.U) {
          tri_ena := true.B
          dm0_ena := true.B
          tri_wea := 1.U
          dm0_wea := 1.U
          tri_addra := 3.U
          dm0_addra := 2.U
          tri_dina(0) := innerloop.io.out_s(0)
          dm0_dina(0) := innerloop.io.out_s(1)
        }.elsewhen(counter4 === 1.U) {
          tri_ena := false.B
          dm0_ena := true.B
          dm0_wea := 1.U
          dm0_addra := 3.U
          dm0_dina(0) := innerloop.io.out_s(0)
        }
      }
    }

    when (c2_done_reg) {

      counter3 := counter3 + 1.U

      when(counter3 === 0.U) {
        tri_enb := true.B
        dm0_enb := true.B
        tri_addrb := 3.U
        dm0_addrb := 2.U

      }.elsewhen(counter3 === 1.U) {
        tri_enb := false.B
        dm0_enb := true.B
        dm0_addrb := 3.U

      }.otherwise {
        tri_enb := false.B
        dm0_enb := false.B

      }

      val read_valid = RegNext(tri_enb || dm0_enb)
      val read_counter = RegNext(counter3)

      when (counter3 >= 1.U) {
        outerloop.io.in_valid := true.B
        outerloop.io.in_en := true.B
      }
      outerloop.io.counter_reset := false.B

      when(read_valid) {
        when(read_counter === 0.U) {
          outerloop.io.xk_in(0) := tri_doutb(0)
          outerloop.io.xk_in(1) := dm0_doutb(0)
        }.otherwise {
          outerloop.io.xk_in(0) := dm0_doutb(0)
          outerloop.io.xk_in(1) := 0.U
        }
      }.otherwise {
        outerloop.io.xk_in(0) := 0.U
        outerloop.io.xk_in(1) := 0.U
      }


    }

    io.r := tri.io.doutb
  }
}