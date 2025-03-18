// ###### DEPR, use main runner instead #########
// ##############################################
////> using scala "2.13.12"
////> using dep "org.chipsalliance::chisel:6.5.0"
////> using plugin "org.chipsalliance:::chisel-plugin:6.5.0"
////> using options "-unchecked", "-deprecation", "-language:reflectiveCalls", "-feature", "-Xcheckinit", "-Xfatal-warnings", "-Ywarn-dead-code", "-Ymacro-annotations"
//package tech.rocksavage.chiselware.I2C
//
//import _root_.circt.stage.ChiselStage
//import _root_.circt.stage.FirtoolOption
//// third-party imports
//import chisel3._
//
//object Main extends App {
//
//  // ######### Getting Setup #########
//  // setting file output directory
//  var output = sys.env.get("BUILD_ROOT")
//  if (output == null) {
//    println("BUILD_ROOT not set, please set and run again")
//    System.exit(1)
//  }
//  // set output directory
//  val outputUnwrapped = output.get
//  val outputDir = s"$outputUnwrapped/verilog"
//
//  val myParams = BaseParams(
//    dataWidth = 8,
//    addrWidth = 8,
//    regWidth = 8,
//    wordWidth = 8,
//    clkFreq = 50
//  )
//  // if output dir does not exist, make path
//  val javaOutputDir = new java.io.File(outputDir)
//  if (!javaOutputDir.exists) javaOutputDir.mkdirs
//
//  // ######### Set Up Top Module HERE #########
//  val top_name = "I2C.sv"
//  val verilog = ChiselStage.emitSystemVerilog(
//    new I2C(myParams),
//    firtoolOpts = Array(
//      "--lowering-options=disallowLocalVariables,disallowPackedArrays",
//      "--disable-all-randomization",
//      "--strip-debug-info",
//      "--split-verilog",
//      s"-o=$outputDir/",
//    )
//  )
//  // ##########################################
//  System.exit(0)
//}
