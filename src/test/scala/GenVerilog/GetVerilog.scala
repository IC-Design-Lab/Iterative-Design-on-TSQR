import Complex_Modules.complex_adder
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.WriteVcdAnnotation
import chiseltest.VerilatorBackendAnnotation
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import FP_Modules.FPUnits.FP_div
//import New_FPU_Mario.FPUnits._
//import Complex_FPU._

import iterative_tsqr._





object main3 extends App {
  (new ChiselStage).execute(
    Array("--target", "systemverilog", "--target-dir", "verification/dut"),
    Seq(ChiselGeneratorAnnotation(() => new complex_adder(64, 13)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("-strip-debug-info")
    )
  )
}

object main4 extends App {
  (new ChiselStage).execute(
    Array("--target", "systemverilog", "--target-dir", "verification/dut"),
    Seq(ChiselGeneratorAnnotation(() => new FP_div(32, 10)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("-strip-debug-info")
    )
  )
}

object main5 extends App {
  (new ChiselStage).execute(
    Array("--target", "systemverilog", "--target-dir", "verification/dut"),
    Seq(ChiselGeneratorAnnotation(() => new tsqr_outer_loop(64, 2, 4, 10, 13, 10, 15)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("-strip-debug-info")
    )
  )
}

object main6 extends App {
  (new ChiselStage).execute(
    Array("--target", "systemverilog", "--target-dir", "verification/dut"),
    Seq(ChiselGeneratorAnnotation(() => new tsqr_inner_loop(64, 2, 4, 2, 10, 13)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("-strip-debug-info")
    )
  )
}