// (c) 2024 Rocksavage Technology, Inc.
// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)
package tech.rocksavage.chiselware.I2C

import java.io.File
import scala.util.Random

import org.scalatest.Assertions._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.coverage._
import chiseltest.simulator.VerilatorFlags
import firrtl2.AnnotationSeq
import firrtl2.annotations.Annotation
import firrtl2.options.TargetDirAnnotation

import TestUtils.checkCoverage
import TestUtils.randData

class I2CTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers
    with ApbUtils {

  val numTests     = 2
  val testName     = System.getProperty("testName")
  println(s"Running test: $testName")

  // Command-line toggles, e.g. -DenableVcd=true, etc.
  val enableVcd    = System.getProperty("enableVcd", "false").toBoolean
  val enableFst    = System.getProperty("enableFst", "true").toBoolean
  val useVerilator = System.getProperty("useVerilator", "false").toBoolean

  val buildRoot    = sys.env.get("BUILD_ROOT_RELATIVE")
  if (buildRoot.isEmpty) {
    println("BUILD_ROOT_RELATIVE not set. Please set and rerun.")
    System.exit(1)
  }
  val testDir = buildRoot.get + "/test"

  val backendAnnotations = {
    var annos: Seq[Annotation] = Seq()
    if (enableVcd) annos = annos :+ WriteVcdAnnotation
    if (enableFst) annos = annos :+ WriteFstAnnotation
    if (useVerilator) {
      annos = annos :+ VerilatorBackendAnnotation
      annos = annos :+ VerilatorFlags(Seq("--cc", "--std=c++17"))
    }
    annos = annos :+ TargetDirAnnotation(testDir)
    annos
  }

  // Decide which test to run based on "testName"
  if (testName == "regression") {
    (1 to numTests).foreach { config =>
      runTest(s"I2C_test_config_$config")
    }
  } else {
    // Single test
    runTest(testName)
  }

  def runTest(testName: String): Unit = {
    behavior of testName

    // Example I2C parameters
    val myParams = BaseParams(dataWidth = 8, addrWidth = 16, regWidth = 8, coverage = false)
    info(s"Data Width: ${myParams.dataWidth}, Address Width: ${myParams.addrWidth}")
    info("--------------------------------")

    testName match {
      case "masterClock" =>
        it should "generate the correct clock frequency for master mode" in {
          val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
            clockTests.masterClock(dut, myParams)
          }
          coverageCollection(cov.getAnnotationSeq, myParams, testName)
        }

      case "masterAddressTransmit" =>
        it should "correctly transmit address packets in master mode" in {
          val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
            transmitTests.masterAddressTransmit(dut, myParams)
          }
          coverageCollection(cov.getAnnotationSeq, myParams, testName)
        }

      case "masterDataTransmit" =>
        it should "correctly transmit data packets in master mode" in {
          val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
            transmitTests.masterDataTransmit(dut, myParams)
          }
          coverageCollection(cov.getAnnotationSeq, myParams, testName)
        }

      case "masterDataReceive" =>
        it should "correctly receive data packets in master mode" in {
          val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
            receiveTests.masterDataReceive(dut, myParams)
          }
          coverageCollection(cov.getAnnotationSeq, myParams, testName)
        }

      case "slaveAddressReceive" =>
        it should "correctly receive address packets in slave mode" in {
          val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
            receiveTests.slaveAddressReceive(dut, myParams)
          }
          coverageCollection(cov.getAnnotationSeq, myParams, testName)
        }

      case "slaveDataReceive" =>
        it should "correctly receive data packets in slave mode" in {
          val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
            receiveTests.slaveDataReceive(dut, myParams)
          }
          coverageCollection(cov.getAnnotationSeq, myParams, testName)
        }

      case "slaveDataTransmit" =>
        it should "correctly transmit data packets in slave mode" in {
          val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
            transmitTests.slaveDataTransmit(dut, myParams)
          }
          coverageCollection(cov.getAnnotationSeq, myParams, testName)
        }

      case "allTests" =>
        runAllTests(myParams)

      case _ =>
        runAllTests(myParams)
    }

    it should "generate cumulative coverage report" in {
    coverageCollector.saveCumulativeCoverage(myParams)
    }
  }

  // ------------------------------------------------
  // Run all major tests
  // ------------------------------------------------
  def runAllTests(myParams: BaseParams): Unit = {
    clockTestsFull(myParams)
    transmitTestsFull(myParams)
    receiveTestsFull(myParams)
  }

  def clockTestsFull(myParams: BaseParams): Unit = {
    it should "generate the correct clock frequency for master mode" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        clockTests.masterClock(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "masterClock")
    }
  }

  def transmitTestsFull(myParams: BaseParams): Unit = {
    it should "correctly transmit address packets in master mode" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        transmitTests.masterAddressTransmit(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "masterAddressTransmit")
    }

    it should "correctly transmit data packets in master mode" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        transmitTests.masterDataTransmit(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "masterDataTransmit")
    }

    it should "correctly transmit data packets in slave mode" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        transmitTests.slaveDataTransmit(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "slaveDataTransmit")
    }
  }

  def receiveTestsFull(myParams: BaseParams): Unit = {
    it should "correctly receive data packets in master mode" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        receiveTests.masterDataReceive(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "masterDataReceive")
    }

    it should "correctly receive address packets in slave mode" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        receiveTests.slaveAddressReceive(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "slaveAddressReceive")
    }

    it should "correctly receive data packets in slave mode" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        receiveTests.slaveDataReceive(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "slaveDataReceive")
    }
  }

  // ------------------------------------------------
  // Collect coverage data
  // ------------------------------------------------
  def coverageCollection(
      cov: Seq[Annotation],
      myParams: BaseParams,
      testName: String
  ): Unit = {
    if (myParams.coverage) {
      val coverage = cov
        .collectFirst { case a: TestCoverage => a.counts }
        .get
        .toMap

      val testConfig =
        myParams.addrWidth.toString + "_" + myParams.dataWidth.toString

      val buildRoot = sys.env.get("BUILD_ROOT")
      if (buildRoot.isEmpty) {
        println("BUILD_ROOT not set, please set and run again")
        System.exit(1)
      }
      val scalaCoverageDir = new File(buildRoot.get + "/cov/scala")
      val verCoverageDir   = new File(buildRoot.get + "/cov/verilog")
      verCoverageDir.mkdirs()
      val coverageFile = verCoverageDir.toString + "/" + testName + "_" +
        testConfig + ".cov"

      val stuckAtFault = checkCoverage(coverage, coverageFile)
      if (stuckAtFault)
        println(
          s"WARNING: At least one IO port did not toggle -- see $coverageFile"
        )
      info(s"Verilog Coverage report written to $coverageFile")
    }
  }
}
