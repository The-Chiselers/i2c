package tech.rocksavage.chiselware.I2C

import java.io.{File, PrintWriter}
import scala.collection.mutable
import firrtl2.AnnotationSeq
import firrtl2.annotations.Annotation // Correct Annotation type for firrtl2
import firrtl2.options.TargetDirAnnotation
import TestUtils.checkCoverage
import TestUtils.randData
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.coverage._
import chiseltest.simulator._

object coverageCollector {
  private val cumulativeCoverage: mutable.Map[String, BigInt] = mutable.Map()

def collectCoverage(
    cov: Seq[Annotation],
    myParams: BaseParams,
    testName: String
): Unit = {
  if (myParams.coverage) {
    val coverage = cov
      .collectFirst { case a: TestCoverage => a.counts }
      .getOrElse(Map.empty)
      .toMap

    // Convert Map[String, Long] to Map[String, BigInt]
    val bigIntCoverage = coverage.map { case (key, value) => key -> BigInt(value) }

    // Merge the test coverage into the cumulative coverage
    for ((key, value) <- bigIntCoverage) {
      cumulativeCoverage.update(key, cumulativeCoverage.getOrElse(key, BigInt(0)) + value)
    }

    val testConfig =
      myParams.addrWidth.toString + "_" + myParams.dataWidth.toString

    val buildRoot = sys.env.get("BUILD_ROOT")
    if (buildRoot.isEmpty) {
      println("BUILD_ROOT not set, please set and run again")
      System.exit(1)
    }

    val verCoverageDir = new File(buildRoot.get + "/cov/verilog")
    verCoverageDir.mkdirs()
    val coverageFile = verCoverageDir.toString + "/" + testName + "_" +
      testConfig + ".cov"

    // Save individual test coverage
    saveCoverageToFile(bigIntCoverage, coverageFile)

    val stuckAtFault = TestUtils.checkCoverage(bigIntCoverage.map { case (k, v) => k -> v.toLong }.toMap, coverageFile)
    if (stuckAtFault)
      println(
        s"WARNING: At least one IO port did not toggle -- see $coverageFile"
      )
    info(s"Verilog Coverage report written to $coverageFile")
  }
}


  def saveCumulativeCoverage(myParams: BaseParams): Unit = {
    if (myParams.coverage) {
      val buildRoot = sys.env.get("BUILD_ROOT")
      if (buildRoot.isEmpty) {
        println("BUILD_ROOT not set, please set and run again")
        System.exit(1)
      }

      val verCoverageDir = new File(buildRoot.get + "/cov/verilog")
      val cumulativeFile = verCoverageDir.toString + "/cumulative_coverage.cov"

      // Write the cumulative coverage to a file
      saveCoverageToFile(cumulativeCoverage.toMap, cumulativeFile)
      info(s"Cumulative coverage report written to $cumulativeFile")
    }
  }

  private def saveCoverageToFile(coverage: Map[String, BigInt], filePath: String): Unit = {
    val writer = new PrintWriter(new File(filePath))
    try {
      for ((key, value) <- coverage) {
        writer.println(s"$key: $value")
      }
    } finally {
      writer.close()
    }
  }

  private def info(message: String): Unit = {
    println(message)
  }
}



