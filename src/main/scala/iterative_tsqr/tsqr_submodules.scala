package iterative_tsqr

import chisel3._
import chisel3.util.{Cat, ShiftRegister, log2Ceil}
import Binary_Modules.BinaryDesignsNew._
import Complex_Modules._
import FP_Modules.FPUnits._

//sub modules//

class alpha (bw: Int, mult_pd: Int, div_pd: Int, sqrt_pd: Int, add_pd: Int) extends Module {
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

  val sqrt = Module(new FP_sqrt((bw/2), sqrt_pd))
  sqrt.io.in_en := false.B
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
    sqrt.io.in_en := true.B
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

class vk_gen (bw: Int, sw: Int, add_pd: Int) extends Module {
  val io = IO(new Bundle {
    val xk_in = Input(Vec(sw, UInt(bw.W)))
    val alpha_in = Input(UInt(bw.W))
    val k_in = Input(UInt(32.W))
    val in_en = Input(Bool())
    val counter_reset = Input(Bool())
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out_s = Output(Vec(sw, UInt(bw.W)))
  })

  val cycles = ((io.k_in + 1.U) >> 1).asUInt
  val counter = RegInit(0.U(32.W))
  when (io.counter_reset) {
    counter := 0.U
  }.otherwise {
    counter := Mux(io.in_en, counter + 1.U, counter)
  }

  val adder = Module(new complex_adder (bw, add_pd))
  adder.io.in_en := false.B
  adder.io.in_valid := false.B
  adder.io.complexA := 0.U
  adder.io.complexB := 0.U

  when(counter >= 0.U) {
    adder.io.in_en := true.B
    adder.io.in_valid := true.B
    when(counter === 0.U) {
      adder.io.complexA := io.xk_in(0)
      adder.io.complexB := io.alpha_in
    }
  }

  val shifted_xk = VecInit(Seq.tabulate(sw)(i => ShiftRegister(io.xk_in(i), add_pd)))
  val outVec = Wire(Vec(sw, UInt(bw.W)))

  for (i <- 0 until sw) {
    outVec(i) := Mux(i.U === 0.U && counter === add_pd.U, adder.io.out_s, shifted_xk(i))
  }

  io.out_s := outVec

  val out_valid = WireDefault(false.B)
  when(counter >= add_pd.U && counter < (cycles + add_pd.U)) {
    out_valid := true.B
  }

  io.out_valid := out_valid
}

class tk_gen (bw: Int, sw: Int, k: Int, mult_pd: Int, add_pd: Int, div_pd: Int) extends Module {
  val io = IO(new Bundle {
    val vk_in = Input(Vec(sw, UInt(bw.W)))
    val in_en = Input(Bool())
    val counter_reset = Input(Bool())
    val in_valid = Input(Bool())
    //val out_valid = Output(Bool())
    val out_s = Output(UInt((bw/2).W))
  })

  val num_batches = k / sw //((k + (n - 1)) / n)
  val num_acc = log2Ceil(num_batches)
  val mult_latency = (mult_pd + add_pd) + (log2Ceil(sw) * add_pd)
  val dot_latency = mult_latency + (num_acc * add_pd) + (math.pow(2, (num_acc)).toInt - 1)
  val latency = dot_latency + div_pd + 1

  val out_s_reg = RegInit(0.U((bw/2).W))
  io.out_s := out_s_reg

  val dot = Module(new cmplx_dot_iterative(sw, k, bw, mult_pd, add_pd))
  dot.io.vec_a := VecInit(Seq.fill(sw)(0.U(bw.W)))
  dot.io.vec_b := VecInit(Seq.fill(sw)(0.U(bw.W)))
  dot.io.in_en := false.B
  dot.io.counter_reset := false.B
  dot.io.in_valid := false.B

  val divider = Module(new FP_div((bw/2), div_pd))
  divider.io.in_a := 0.U
  divider.io.in_b := 0.U
  divider.io.in_en := false.B
  divider.io.in_valid := false.B

  val counter = RegInit(0.U(32.W))
  when (io.counter_reset) {
    counter := 0.U
  }.otherwise {
    counter := Mux(io.in_en, counter + 1.U, counter)
  }

  when(io.in_en) {
    dot.io.in_en := true.B
    dot.io.in_valid := true.B
    dot.io.counter_reset := io.counter_reset
    dot.io.vec_a := io.vk_in
    dot.io.vec_b := io.vk_in
  }

  when(counter >= dot_latency.U){
    divider.io.in_en := true.B
    divider.io.in_valid := true.B
    divider.io.in_a := "hc0000000".U
    divider.io.in_b := dot.io.out_real
  }

  when(counter === (latency - 1).U){
    out_s_reg := divider.io.out_s
  }
}

//outer loop top module//

class tsqr_outer_loop (bw: Int, sw: Int, k: Int, mult_pd: Int, add_pd: Int, div_pd: Int, sqrt_pd: Int) extends Module {
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

  val alpha_gen = Module(new alpha (bw, mult_pd, div_pd, sqrt_pd, add_pd))
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

  val sqrt = Module(new FP_sqrt((bw/2), sqrt_pd))
  sqrt.io.in_en := false.B
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
      sqrt.io.in_en := true.B
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

  //  when(io.counter_reset) {
  //    valid_done := false.B
  //  }

  io.alpha_out := alpha_reg//Mux(counter === alpha_done.U, alpha_gen.io.out_s, 0.U)
  //io.out_vk := Mux(counter >= vk_ready.U && counter < (vk_ready + num_batches).U, vk_gen.io.out_s, VecInit.fill(sw)(0.U(bw.W)))
  io.out_tk :=  tk_reg//Mux(counter === tk_done.U, tk_gen.io.out_s, 0.U)
  //io.out_valid :=  valid_done//Mux(counter === tk_done.U, true.B, 0.U)


}

//inner loop top module//

class tsqr_inner_loop (bw: Int, sw: Int, k: Int, c: Int, mult_pd: Int, add_pd: Int ) extends Module {
  val io = IO(new Bundle {
    val xk_in = Input(Vec(sw, UInt(bw.W)))
    val alpha_in = Input(UInt(bw.W))
    val tk_in = Input(UInt((bw/2).W))
    val column_count = Input(UInt(log2Ceil(c + 1).W))
    val en_in = Input(Bool())
    val valid_in = Input(Bool())
    val counter_reset = Input(Bool())
    val valid_out = Output(Bool())
    val col2_done = Output(Bool())
    val updates_done = Output(Bool())
    val out_s = Output(Vec(sw, UInt(bw.W)))


  })

  val num_batches = k / sw //((k + (n - 1)) / n)
  val num_acc = log2Ceil(num_batches)
  val mult_latency = (mult_pd + add_pd) + (log2Ceil(sw) * add_pd)
  val dot_latency = mult_latency + (num_acc * add_pd) + (math.pow(2, (num_acc)).toInt - 1)

  val vk_latency = add_pd

  val dot_done = vk_latency + dot_latency

  val tk_scalar_done = dot_done + mult_pd + 1

  val axpy_latency = (mult_pd + add_pd) + add_pd

  val latency = tk_scalar_done + axpy_latency

  val vk_reg = RegInit(VecInit(Seq.fill(k)(0.U(bw.W))))

  val tk_reg = RegInit(0.U((bw/2).W))


  //val tk_complex = Cat(io.tk_in, 0.U(32.W))

  val counter = RegInit(0.U(32.W))
  when(io.counter_reset) {
    counter := 0.U
  }.otherwise {
    counter := Mux(io.en_in, counter + 1.U, counter)
  }

  when (io.en_in && counter === 0.U) {
    tk_reg := io.tk_in
  }

  val k_u = k.U
  val vk = Module(new vk_gen(bw, sw, add_pd))
  vk.io.xk_in := VecInit.fill(sw)(0.U(bw.W))
  vk.io.alpha_in := 0.U
  vk.io.k_in := k_u
  vk.io.in_en := false.B
  vk.io.in_valid := false.B
  vk.io.counter_reset := false.B

  val dot = Module(new cmplx_dot_iterative(sw, k, bw, mult_pd, add_pd ))
  dot.io.vec_a := VecInit.fill(sw)(0.U(bw.W))
  dot.io.vec_b := VecInit.fill(sw)(0.U(bw.W))
  dot.io.in_en := false.B
  dot.io.in_valid := false.B
  dot.io.counter_reset := false.B

  val mult = Seq.fill(2)(Module(new FP_mult(bw/2, mult_pd)))

  for (m <- mult) {
    m.io.in_en := io.en_in
    m.io.in_valid := io.valid_in
    m.io.in_a := 0.U
    m.io.in_b := 0.U
  }

  val axpy = Module(new iterative_axpy(bw, sw, k, mult_pd, add_pd))
  axpy.io.xk_in := VecInit.fill(sw)(0.U(bw.W))
  axpy.io.vk_in := VecInit.fill(sw)(0.U(bw.W))
  axpy.io.counter_reset := false.B
  axpy.io.s_in := 0.U
  axpy.io.en_in := false.B
  axpy.io.valid_in := false.B

  when(io.en_in) {
    vk.io.in_en := true.B
    vk.io.in_valid := true.B
    vk.io.counter_reset := io.counter_reset

    vk.io.xk_in := io.xk_in
    vk.io.alpha_in := io.alpha_in

  }

  val vk_reg_pointer = RegInit(0.U(log2Ceil(k).W))
  when(counter >= vk_latency.U && counter < (vk_latency + num_batches).U) {
    for (i <- 0 until sw) {
      val index = vk_reg_pointer + i.U
      when(index < k.U) {
        vk_reg(index) := vk.io.out_s(i)
      }
    }
    vk_reg_pointer := vk_reg_pointer + sw.U
  }

  when (io.counter_reset) {
    vk_reg_pointer := 0.U
  }


  val vk_out_pointer = RegInit(0.U(log2Ceil(k).W))
  val vk_out_batch = Wire(Vec(sw, UInt(bw.W)))

  vk_out_batch := VecInit(Seq.fill(sw)(0.U(bw.W)))

  for (i <- 0 until sw) {
    val index = vk_out_pointer + i.U
    val new_index = Mux(index >= k.U, index - k.U, index)
    vk_out_batch(i) := vk_reg(new_index)
  }

  when(counter >= vk_latency.U) {
    dot.io.in_en := true.B
    dot.io.in_valid := true.B
    dot.io.counter_reset := io.counter_reset
    dot.io.vec_b := ShiftRegister(io.xk_in, add_pd)
    when (counter < (vk_latency + num_batches).U) {
      dot.io.vec_a := vk.io.out_s
      //dot.io.vec_b := ShiftRegister(io.xk_in, add_pd)
    }.otherwise {
      for (i <- 0 until sw) {
        dot.io.vec_a(i) := vk_out_batch(i)
      }


      val next_pointer = vk_out_pointer + sw.U
      vk_out_pointer := Mux(next_pointer >= k.U, next_pointer - k.U, next_pointer)
    }
  }



  when (counter >= dot_done.U) {
    mult(0).io.in_en := true.B
    mult(0).io.in_valid := true.B

    mult(0).io.in_a := tk_reg
    mult(0).io.in_b := dot.io.out_real

    mult(1).io.in_en := true.B
    mult(1).io.in_valid := true.B

    mult(1).io.in_a := tk_reg
    mult(1).io.in_b := dot.io.out_imag

  }

  val tk_scalar = Cat(mult(0).io.out_s, mult(1).io.out_s)
  val scalar_reg = RegInit(0.U(bw.W))
  //  when (counter === (tk_scalar_done - 1).U) {
  //    scalar_reg := tk_scalar
  //  }

  for(i <- 0 until c) {
    when (counter >= (tk_scalar_done - 1).U && counter === ((tk_scalar_done - 1) + (num_batches * i)).U) {
      scalar_reg := tk_scalar
    }
  }



  val vk_out_pointer2 = RegInit(0.U(log2Ceil(k).W))
  val vk_out_batch2 = Wire(Vec(sw, UInt(bw.W)))

  vk_out_batch2 := VecInit(Seq.fill(sw)(0.U(bw.W)))


  for (i <- 0 until sw) {
    val index2 = vk_out_pointer2 + i.U
    val new_index2 = Mux(index2 >= k.U, index2 - k.U, index2)
    vk_out_batch2(i) := vk_reg(new_index2)
  }


  when (counter >= tk_scalar_done.U) {
    axpy.io.en_in := true.B
    axpy.io.valid_in := true.B
    axpy.io.s_in := scalar_reg
    axpy.io.xk_in := ShiftRegister(io.xk_in, tk_scalar_done)
    //axpy.io.vk_in := ShiftRegister(vk.io.out_s, (dot_latency + mult_pd + 1))

    for (i <- 0 until sw) {
      axpy.io.vk_in(i) := vk_out_batch2(i)
    }


    val next_pointer2 = vk_out_pointer2 + sw.U
    vk_out_pointer2 := Mux(next_pointer2 >= k.U, next_pointer2 - k.U, next_pointer2)
  }

  when (io.counter_reset) {
    vk_out_pointer := 0.U
    vk_out_pointer2 := 0.U // added this 2/6/26
  }

  io.out_s := axpy.io.out_s


  // new control logic
  val colCountLast = RegInit(0.U(io.column_count.getWidth.W))
  val colLatched = RegInit(false.B)
  when(io.counter_reset) {
    colCountLast := 0.U
    colLatched := false.B
  }.elsewhen(io.en_in && !colLatched) {
    colCountLast := io.column_count
    colLatched := true.B
  }



  val start = latency.U(32.W)
  val total_batches = Wire(UInt(32.W))
  total_batches := (colCountLast * num_batches.U).asUInt
  val end_of_total_valid = Wire(UInt(32.W))
  end_of_total_valid := start + total_batches
  val end_of_updates = end_of_total_valid - 1.U

  io.valid_out := (counter >= start) && (counter < end_of_total_valid)
  io.updates_done := (counter === end_of_updates)

  val col2_end = start + (num_batches.U * 2.U) - 1.U
  io.col2_done := (colCountLast >= 2.U) && (counter === col2_end)


}