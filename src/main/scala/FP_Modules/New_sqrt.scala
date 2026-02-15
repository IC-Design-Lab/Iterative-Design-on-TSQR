package FP_Modules

import Binary_Modules.BinaryDesignsNew._
import chisel3._
import chisel3.util.{Counter, ShiftRegister, log2Ceil, Pipe}
import FormatConvert.convert_string_to_IEEE_754

class frac_sqrt2(bw: Int, L: Int, latency: Int) extends Module{
  val io = IO(new Bundle(){
    val in_ready = Output(Bool())
    val out_ready = Input(Bool())
    val in_a = Input(UInt((bw+2).W))
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out_s = Output(UInt(bw.W))
  })
  override def desiredName = s"frac_sqrt_BW${bw}_${latency}"
  val pipe_skip = if (latency >= L) 1 else  L / latency
  val pipe_map = Array.fill(L)(0)
  for(i <- 0 until latency){
    pipe_map((i*pipe_skip) % L) += 1
  }

  // Pipeline enable: advance when output is consumed or no valid output
  val pipe_enable = io.out_ready || !io.out_valid

  val P_wires = WireDefault(VecInit.fill(L-1)(0.U((bw*2+1).W)))
  val X_wires = WireDefault(VecInit.fill(L-1)(0.U((bw*2+2).W)))
  val result_wires = WireDefault(VecInit.fill(L)(0.U(bw.W)))
  val ovalid_wires = Wire(Vec(L, Bool()))

  val P_pipeline = P_wires.zip(ovalid_wires.slice(0,L-1)).zip(pipe_map).map(i=>Pipe(i._1._2 && pipe_enable,i._1._1,i._2))
  val X_pipeline = X_wires.zip(ovalid_wires.slice(0,L-1)).zip(pipe_map).map(i=>Pipe(i._1._2 && pipe_enable,i._1._1,i._2))
  val result_pipeline = result_wires.zip(ovalid_wires).zip(pipe_map).map(i=>Pipe(i._1._2 && pipe_enable,i._1._1,i._2))

  val one = 1.U(1.W) ## 0.U((bw*2).W)
  val shifted_sqrd_ones = Wire(Vec(L, UInt((bw*2+1).W)))
  shifted_sqrd_ones.zipWithIndex.foreach(x=> x._1 := (one >> ((x._2 + 1)*2)))
  val shifted_ones = Wire(Vec(L, UInt((bw*2+1).W)))
  shifted_ones.zipWithIndex.foreach(x=> x._1 := (one >> (x._2 + 1)))
  val shifted_ps = Wire(Vec(L-1, UInt((bw*2+1).W)))
  shifted_ps.zipWithIndex.foreach(x=>x._1 := (P_pipeline(x._2).bits >> (x._2+1)))
  io.out_valid := result_pipeline.last.valid
  io.out_s := result_pipeline.last.bits
  val in = (io.in_a ## 0.U(bw.W)) - one
  // Ready when pipeline can advance
  io.in_ready := pipe_enable
  // Input enters pipeline on valid handshake
  ovalid_wires(0) := io.in_valid && io.in_ready
  for(i <- 1 until L) ovalid_wires(i) := result_pipeline(i-1).valid
  //    printf(p"results: ${results}\n")
  for(i <- 0 until L){
    if(i == 0){
      val shifted_one = shifted_sqrd_ones(i)((bw*2)-1,0)
      val y = shifted_one +& one
      val yleqx = y <= in
      P_wires(0) := Mux(yleqx, shifted_ones(i) + one, one)
      X_wires(0) := Mux(yleqx, in - y, in)
      val t = VecInit(0.U(bw.W).asBools)
      t(bw-1) := yleqx
      result_wires(0) := t.asUInt
    }else{
      val shifted_one = shifted_sqrd_ones(i)((bw*2)-1,0)
      val shifted_P = shifted_ps(i-1)
      val y = shifted_P +& shifted_one
      val yleqx = y <= X_pipeline(i-1).bits
      if(i != L - 1) {
        P_wires(i) := Mux(yleqx, shifted_ones(i) + P_pipeline(i-1).bits, P_pipeline(i-1).bits)
        X_wires(i) := Mux(yleqx, X_pipeline(i-1).bits - y, X_pipeline(i-1).bits)
      }
      val t = VecInit(result_pipeline(i-1).bits.asBools)
      t(bw-1 - i):= yleqx
      result_wires(i) := t.asUInt
    }
  }
}


// fp_sqrt new design
class FP_sqrt2(bw: Int, L: Int, latency: Int) extends Module {
  val (exponent, mantissa) = bw match {
    case 16 => (5,10)
    case 32 => (8,23)
    case 64 => (11,52)
    case 128 => (15,112)
  }
  require((bw == 16 || bw == 32 || bw == 64 || bw == 128) && L <= mantissa)
  val io = IO(new Bundle() {
    val in_ready = Output(Bool())
    val out_ready = Input(Bool())
    val in_valid = Input(Bool())
    val in_a = Input(UInt(bw.W))
    val out_s = Output(UInt(bw.W))
    val out_valid = Output(Bool())
  })
  override def desiredName = s"FP_sqrt_${bw}_${L}_${latency}"

  // Instantiate sqrt first to get its ready signal
  val fsqrt = Module(new frac_sqrt2(mantissa,L, latency)).io
  fsqrt.out_ready := io.out_ready

  // Pipeline enable derived from sqrt's ready
  val pipe_enable = fsqrt.in_ready

  // Propagate ready signal
  io.in_ready := fsqrt.in_ready

  val bias = (BigInt(2).pow(exponent - 1) - 1).U(exponent.W)
  val sign = io.in_a(bw - 1)
  val exp = io.in_a(bw-2, mantissa) -& bias
  val frac = 1.U(2.W) ## io.in_a(mantissa - 1, 0)

  val exp_new = Mux(exp(0).asBool,(exp - 1.U) >> 1, exp >> 1).asUInt

  val ref_frac = Mux(exp(0).asBool, frac << 1, frac).asUInt

  fsqrt.in_valid := io.in_valid
  fsqrt.in_a := ref_frac

  val out_sign = ShiftRegister(sign, latency, pipe_enable)
  val out_exp = ShiftRegister(exp_new, latency, pipe_enable) + bias
  val out_frac = fsqrt.out_s

  io.out_valid := fsqrt.out_valid
  io.out_s := out_sign ## out_exp(exponent - 1, 0) ## out_frac
}