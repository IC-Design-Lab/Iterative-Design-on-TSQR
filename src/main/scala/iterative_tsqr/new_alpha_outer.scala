package iterative_tsqr

import chisel3._
import chisel3.util.{Cat, ShiftRegister, log2Ceil}
import Binary_Modules.BinaryDesignsNew._
import Complex_Modules._
import FP_Modules.FPUnits._
import FP_Modules.FP_sqrt2

class alpha2 (bw: Int, mult_pd: Int, div_pd: Int, sqrt_pd: Int, add_pd: Int) extends Module {
  val io = IO(new Bundle{
    val in_x0 = Input(UInt(bw.W))
    val in_eu_norm = Input(UInt((bw/2).W))
    val in_en = Input(Bool())
    val counter_reset = Input(Bool())
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out_s = Output(UInt(bw.W))
    val out_real = Output(UInt((bw/2).W))
    val out_imag = Output(UInt((bw/2).W))
  })

  val latency = mult_pd + add_pd + sqrt_pd + div_pd + mult_pd
  val mult_done = mult_pd
  val add_done = mult_done + add_pd
  val sqrt_done = add_done + sqrt_pd
  val div_done = sqrt_done + div_pd
  val final_done = div_done + mult_pd

  val counter = RegInit(0.U(32.W))
  when (io.counter_reset) {
    counter := 0.U
  }.elsewhen(io.in_en && counter <= latency.U) {
    counter := counter + 1.U
  }

  val eu_norm_reg = RegInit(0.U(32.W))
  when(counter === 0.U) {
    eu_norm_reg := io.in_eu_norm
  }

  io.out_s := 0.U
  io.out_real := 0.U
  io.out_imag := 0.U
  io.out_valid := false.B

  val mult = Seq.fill(4)(Module(new FP_mult( (bw/2), mult_pd)))

  for (m <- mult) {
    m.io.in_en := false.B
    m.io.in_valid := false.B
    m.io.in_a := 0.U
    m.io.in_b := 0.U
  }

  val adder = Module(new FP_add((bw/2), add_pd))
  adder.io.in_en := false.B
  adder.io.in_valid := false.B
  adder.io.in_a := 0.U
  adder.io.in_b := 0.U

  val sqrt = Module(new FP_sqrt2((bw/2),sqrt_pd, sqrt_pd))
  //sqrt.io.in_en := false.B
  sqrt.io.out_ready := false.B
  sqrt.io.in_valid := false.B
  sqrt.io.in_a := 0.U

  val divider = Seq.fill(2)(Module(new FP_div((bw/2), div_pd)))

  for (d <- divider) {
    d.io.in_en := false.B
    d.io.in_valid := false.B
    d.io.in_a := 0.U
    d.io.in_b := 0.U
  }

  when(io.in_en){
    mult(0).io.in_en := true.B
    mult(0).io.in_valid := true.B
    mult(1).io.in_en := true.B
    mult(1).io.in_valid := true.B

    mult(0).io.in_a := io.in_x0((bw - 1), (bw /2))
    mult(0).io.in_b := io.in_x0((bw - 1), (bw /2))

    mult(1).io.in_a := io.in_x0(((bw /2) - 1), 0)
    mult(1).io.in_b := io.in_x0(((bw /2) - 1), 0)
  }

  when(counter >= mult_done.U) {
    adder.io.in_en := true.B
    adder.io.in_valid := true.B
    adder.io.in_a := mult(0).io.out_s
    adder.io.in_b := mult(1).io.out_s
  }

  when(counter >= add_done.U) {
    //sqrt.io.in_en := true.B
    sqrt.io.out_ready := true.B
    sqrt.io.in_valid := true.B
    sqrt.io.in_a := adder.io.out_s
  }

  val x0_real_shift = ShiftRegister(io.in_x0((bw - 1), (bw /2)), 33, io.in_en)
  val x0_imag_shift = ShiftRegister(io.in_x0(((bw /2) - 1), 0), 33, io.in_en)

  when(counter >= sqrt_done.U) {
    divider(0).io.in_en := true.B
    divider(0).io.in_valid := true.B
    divider(1).io.in_en := true.B
    divider(1).io.in_valid := true.B

    divider(0).io.in_a := x0_real_shift
    divider(0).io.in_b := sqrt.io.out_s

    divider(1).io.in_a := x0_imag_shift
    divider(1).io.in_b := sqrt.io.out_s
  }

  when(counter >= div_done.U){
    mult(2).io.in_en := true.B
    mult(2).io.in_valid := true.B
    mult(3).io.in_en := true.B
    mult(3).io.in_valid := true.B

    mult(2).io.in_a := eu_norm_reg
    mult(2).io.in_b :=  divider(0).io.out_s

    mult(3).io.in_a := eu_norm_reg
    mult(3).io.in_b := divider(1).io.out_s
  }

  io.out_s := Cat(mult(2).io.out_s, mult(3).io.out_s)
  io.out_real := mult(2).io.out_s
  io.out_imag := mult(3).io.out_s
  io.out_valid := ShiftRegister(io.in_valid, latency, io.in_en)

}

class tsqr_outer_loop2 (bw: Int, sw: Int, k: Int, mult_pd: Int, add_pd: Int, div_pd: Int, sqrt_pd: Int) extends Module {
  val io = IO(new Bundle {
    val xk_in = Input(Vec(sw, UInt(bw.W)))
    val in_valid = Input(Bool())
    val counter_reset = Input(Bool())
    val in_en = Input(Bool())
    val alpha_out = Output(UInt(bw.W))
    //val out_vk = Output(Vec(sw, UInt(bw.W)))
    val out_tk = Output(UInt((bw/2).W))
    val out_valid = Output(Bool())
  })



  val num_batches = k / sw //((k + (n - 1)) / n)
  val num_acc = log2Ceil(num_batches)
  val mult_latency = (mult_pd + add_pd) + (log2Ceil(sw) * add_pd)
  val dot_latency = mult_latency + (num_acc * add_pd) + (math.pow(2, (num_acc)).toInt - 1)

  val alpha_latency = mult_pd + add_pd + sqrt_pd + div_pd + mult_pd
  val vk_gen_latency = add_pd
  val tk_gen_latency = dot_latency + div_pd + 1
  val shift1 = (dot_latency + sqrt_pd)  //- (alpha_latency - mult_pd)
  val shift2 = alpha_latency + shift1
  val alpha_done = shift2
  val eu_norm_done = dot_latency + sqrt_pd
  val vk_ready = shift2 + vk_gen_latency
  val tk_done = vk_ready + tk_gen_latency

  val counter = RegInit(0.U(32.W))
  when(io.counter_reset) {
    counter := 0.U
  }.otherwise {
    counter := Mux(io.in_en, counter + 1.U, counter)
  }

  //counter:= Mux(io.in_en, counter + 1.U, counter)

  val eu_norm_dot = Module(new cmplx_dot_iterative (sw, k, bw, mult_pd, add_pd))
  eu_norm_dot.io.in_en := false.B
  eu_norm_dot.io.in_valid := false.B
  eu_norm_dot.io.counter_reset := false.B
  eu_norm_dot.io.vec_a := VecInit.fill(sw)(0.U(bw.W))
  eu_norm_dot.io.vec_b := VecInit.fill(sw)(0.U(bw.W))

  val alpha_gen = Module(new alpha2 (bw, mult_pd, div_pd, sqrt_pd, add_pd))
  alpha_gen.io.in_en := false.B
  alpha_gen.io.in_valid := false.B
  alpha_gen.io.counter_reset := false.B
  alpha_gen.io.in_x0 := 0.U
  alpha_gen.io.in_eu_norm := 0.U

  val height = k.U

  val vk_gen = Module(new vk_gen (bw, sw, add_pd))
  vk_gen.io.in_en := false.B
  vk_gen.io.in_valid := false.B
  vk_gen.io.counter_reset := false.B
  vk_gen.io.xk_in := VecInit.fill(sw)(0.U(bw.W))
  vk_gen.io.k_in := height

  val tk_gen = Module(new tk_gen (bw, sw, k, mult_pd, add_pd, div_pd))
  tk_gen.io.in_en := false.B
  tk_gen.io.in_valid := false.B
  tk_gen.io.counter_reset := false.B
  tk_gen.io.vk_in := VecInit.fill(sw)(0.U(bw.W))

  val sqrt = Module(new FP_sqrt2((bw/2), sqrt_pd, sqrt_pd))
  //sqrt.io.in_en := false.B
  sqrt.io.out_ready := false.B
  sqrt.io.in_valid := false.B
  sqrt.io.in_a := 0.U

  val x0_reg = RegInit(0.U(bw.W))
  when (counter === 0.U) {
    x0_reg := io.xk_in(0)
  }
  val shifted_x0 = ShiftRegister(x0_reg, (shift1 - 1))

  val shifted_xk = Wire(Vec(sw, UInt(bw.W)))
  for (i <- 0 until sw) {
    shifted_xk(i) := ShiftRegister(io.xk_in(i), shift2)
  }
  vk_gen.io.alpha_in  := 0.U((bw).W)
  when(io.in_en) {
    eu_norm_dot.io.in_en := true.B
    eu_norm_dot.io.in_valid := true.B
    eu_norm_dot.io.counter_reset := io.counter_reset

    eu_norm_dot.io.vec_a := io.xk_in
    eu_norm_dot.io.vec_b := io.xk_in

    when(counter >= dot_latency.U) {
      //sqrt.io.in_en := true.B
      sqrt.io.out_ready := true.B
      sqrt.io.in_valid := true.B

      sqrt.io.in_a := eu_norm_dot.io.out_real
    }
  }

  when(counter >= shift1.U) {
    alpha_gen.io.in_en := true.B
    alpha_gen.io.in_valid := true.B
    alpha_gen.io.counter_reset := io.counter_reset

    alpha_gen.io.in_x0 := shifted_x0
    alpha_gen.io.in_eu_norm := sqrt.io.out_s
    //when(counter >= eu_norm_done.U) {
    //  alpha_gen.io.in_eu_norm := sqrt.io.out_s
    //}
  }

  when(counter >= alpha_done.U) {
    vk_gen.io.in_en := true.B
    vk_gen.io.in_valid := true.B
    vk_gen.io.counter_reset := io.counter_reset

    vk_gen.io.alpha_in := alpha_gen.io.out_s
    vk_gen.io.xk_in := shifted_xk
  }

  when(counter >= vk_ready.U) {
    tk_gen.io.in_en := true.B
    tk_gen.io.in_valid := true.B
    tk_gen.io.counter_reset := io.counter_reset

    tk_gen.io.vk_in := vk_gen.io.out_s
  }
  val alpha_reg = RegInit(0.U(bw.W))
  val tk_reg = RegInit(0.U((bw/2).W))

  when (counter === alpha_done.U){
    alpha_reg := alpha_gen.io.out_s
  }
  when (counter === tk_done.U){
    tk_reg := tk_gen.io.out_s
  }

  //val valid_done = RegInit(false.B)

  when (counter === (tk_done + 1).U) {
    io.out_valid := true.B
  }.otherwise {
    io.out_valid := false.B
  }

  //added this
  when(io.counter_reset) {
    alpha_reg := 0.U
    tk_reg := 0.U
  }

  //  when(io.counter_reset) {
  //    valid_done := false.B
  //  }

  io.alpha_out := alpha_reg//Mux(counter === alpha_done.U, alpha_gen.io.out_s, 0.U)
  //io.out_vk := Mux(counter >= vk_ready.U && counter < (vk_ready + num_batches).U, vk_gen.io.out_s, VecInit.fill(sw)(0.U(bw.W)))
  io.out_tk :=  tk_reg//Mux(counter === tk_done.U, tk_gen.io.out_s, 0.U)
  //io.out_valid :=  valid_done//Mux(counter === tk_done.U, true.B, 0.U)


}