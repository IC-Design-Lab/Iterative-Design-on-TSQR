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


object main extends App {
  (new ChiselStage).execute(
    Array("--target", "systemverilog", "--target-dir", "verification/dut"),
    Seq(ChiselGeneratorAnnotation(() => new tsqr_topmodule(64, 1, 2, 4, 2, 10, 13, 10, 15, 4)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("-strip-debug-info")
    )
  )
}

object main2 extends App {
  (new ChiselStage).execute(
    Array("--target", "systemverilog", "--target-dir", "verification/dut"),
    Seq(ChiselGeneratorAnnotation(() => new tsqr_topmodule2(64, 1, 2, 4, 2, 10, 13, 10, 15, 4)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("-strip-debug-info")
    )
  )
}