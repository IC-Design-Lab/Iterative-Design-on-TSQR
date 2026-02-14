package CodeTest

import Complex_Modules.{complex_adder, complex_dot_streaming}
import chisel3._
import chisel3.util.log2Ceil
import chiseltest.{ChiselScalatestTester, _}
import org.scalatest.flatspec.AnyFlatSpec
import iterative_tsqr._
import chiseltest.RawTester.test
import iterative_tsqr._
//import java.util.Random


object tsqr_outer_loop_tb extends App {

  val sw = 2
  val k = 4
  val bw = 64
  val mult_pd = 10
  val add_pd = 13
  val div_pd = 10
  val sqrt_pd = 15

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
  val vk_ready = shift2 + vk_gen_latency
  val tk_done = vk_ready + tk_gen_latency

  test(new tsqr_outer_loop(bw, sw, k, mult_pd, add_pd, div_pd, sqrt_pd)) { c =>

    c.io.counter_reset.poke(false.B)
    c.io.in_en.poke(true.B)
    c.io.in_valid.poke(true.B)

    val inputVec = Seq(
      //4x2 testing
      // column 0
      Seq("h3F80000040000000".U, "hBF80000040000000".U),
      Seq("hBF800000C0000000".U, "hC0000000BF800000".U),

      //column 1 after update
      //Seq("h3f08a23e3fa25a28".U, "h40375d343f887134".U),
      //Seq("h402ed2ec3f08a23e".U, "h0000000000000000".U),

      // 8x2 testing
      // column 0
      //      Seq("h3F8000003F800000".U, "hBF8000003F800000".U),
      //      Seq("h3F800000C0000000".U, "h40000000BF800000".U),
      //      Seq("hC00000003F800000".U, "h3F80000040000000".U),
      //      Seq("hBF800000BF800000".U, "h400000003F800000".U)


      // column 1 after update
      // element 0 already updated is 3f22b300be033910

//      Seq("h3fe964d23f6d19a0".U, "hbf32bb40bf7a41a0".U),
//      Seq("h3f9d2f2f400cbd30".U, "hbf9d2f2f3fe685a0".U),
//      Seq("hc00cbd30bf45a1a2".U, "h4004b998bf969b2f".U),
//      Seq("hbf82df303fa6a260".U, "h0000000000000000".U)





    )



    for (i <- 0 until 400) { // changed from 220
      println(s"===== Cycle $i =====")

      if (i < inputVec.length) {
        c.io.xk_in(0).poke(inputVec(i)(0))
        c.io.xk_in(1).poke(inputVec(i)(1))
      }


      val xkInVals      = c.io.xk_in.map(_.peek().litValue)
      val alphaFinal    = c.io.alpha_out.peek().litValue
      val tkFinal       = c.io.out_tk.peek().litValue


      println(f"out_alpha = 0x$alphaFinal%016x")
      val out3 = c.io.out_tk.peek().litValue
      val out4 = c.io.out_valid.peek().litValue
      println(f"out_tk = 0x$out3%08x")
      println(f"out_valid = $out4")
      c.clock.step(1)
    }
  }
}

object tsqr_inner_loop_tb extends App {

  val sw = 2
  val k = 4
  val c = 2//2
  val bw = 64
  val mult_pd = 10
  val add_pd = 13

  val num_batches = k / sw
  val num_acc = log2Ceil(num_batches)
  val mult_latency = (mult_pd + add_pd) + (log2Ceil(sw) * add_pd)
  val dot_latency = mult_latency + (num_acc * add_pd) + (math.pow(2, num_acc).toInt - 1)
  val vk_latency = add_pd
  val dot_done = vk_latency + dot_latency
  val tk_scalar_done = dot_done + mult_pd
  val total_latency = tk_scalar_done + mult_pd + add_pd + 10 // extra buffer

  test(new tsqr_inner_loop(bw, sw, k, c, mult_pd, add_pd)) { c =>

    c.io.counter_reset.poke(false.B)
    c.io.en_in.poke(true.B)
    c.io.valid_in.poke(true.B)
    c.io.valid_in.poke(false.B)
    c.io.column_count.poke(1.U)


    val all_xk_batches = Seq(
      // 4x2 testing
      // column 0
//            Seq("h3F80000040000000".U, "hBF80000040000000".U),
//            Seq("hBF800000C0000000".U, "hC0000000BF800000".U),
//
//       // column 1
//            Seq("h400000003F800000".U, "h3F80000040000000".U),
//            Seq("h400000003F800000".U, "h400000003F800000".U)

      // after first pass
            Seq("h3f08a23e3fa25a28".U, "h40375d343f887134".U),
            Seq("h402ed2ec3f08a23e".U, "h0000000000000000".U),



      // 8x2 testing
      // column 0
//      Seq("h3F8000003F800000".U, "hBF8000003F800000".U),
//      Seq("h3F800000C0000000".U, "h40000000BF800000".U),
//      Seq("hC00000003F800000".U, "h3F80000040000000".U),
//      Seq("hBF800000BF800000".U, "h400000003F800000".U),
//
//      // column 1
//      Seq("h3F800000BF800000".U, "h400000003F800000".U),
//      Seq("hBF800000BF800000".U, "h3F80000040000000".U),
//      Seq("hBF80000040000000".U, "hC0000000BF800000".U),
//      Seq("h40000000BF800000".U, "hBF8000003F800000".U)

      // after first pass

//            Seq("h3fe964d23f6d19a0".U, "hbf32bb40bf7a41a0".U),
//            Seq("h3f9d2f2f400cbd30".U, "hbf9d2f2f3fe685a0".U),
//            Seq("hc00cbd30bf45a1a2".U, "h4004b998bf969b2f".U),
//            Seq("hbf82df303fa6a260".U, "h0000000000000000".U)
    )
    //    //4x2 testing
//        val alpha = "h3fff7733407f7733".U
//        val tk = "hbd088000".U
//        c.io.tk_in.poke(tk)

    // second pass column 1
        val alpha = "h3fd83996408043b1".U
        val tk = "hbd240000".U
        c.io.tk_in.poke(tk)

    // 8x2 testing
//    val alpha = "h407bf0ce407bf0ce".U // change
//    val tk = "hbcd28000".U //change
//    c.io.tk_in.poke(tk)

    // second pass column 1
//        val alpha = "h409d986c40202ffb".U // change
//        val tk = "hbcc38000".U //change
//        c.io.tk_in.poke(tk)




    for ((batch, cycle) <- all_xk_batches.zipWithIndex) {
      println(s"===== Cycle $cycle (input batch) =====")
      c.io.valid_in.poke(true.B)

      for (j <- 0 until sw) {
        c.io.xk_in(j).poke(batch(j))
      }
      c.io.alpha_in.poke(alpha)



      val outVals = c.io.out_s.map(_.peek().litValue)
      println(f"out_s = Vec(${outVals.map(x => f"0x$x%016x").mkString(", ")})")

      c.clock.step()
    }



    c.io.valid_in.poke(false.B)

    for (i <- 0 until 400) { // changed from 120
      println(s"===== Cycle ${i + all_xk_batches.length} (output phase) =====")

      val outVals    = c.io.out_s.map(_.peek().litValue)
      val validOut   = c.io.valid_out.peek().litToBoolean
      val col2Done   = c.io.col2_done.peek().litToBoolean
      val updatesDone = c.io.updates_done.peek().litToBoolean



      println(f"out_s = Vec(${outVals.map(x => f"0x$x%016x").mkString(", ")})  " +
        f"valid_out = $validOut  col2_done = $col2Done  updates_done = $updatesDone")

      c.clock.step()
    }
  }
}