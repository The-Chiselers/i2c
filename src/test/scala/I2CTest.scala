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
  val enableVcd    = System.getProperty("enableVcd", "true").toBoolean
  val enableFst    = System.getProperty("enableFst", "false").toBoolean
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

  /** A convenience method to run a single named test. */
  def runTest(name: String): Unit = {
    behavior of name

    // Example I2C parameters
    val myParams = BaseParams(dataWidth = 8, addrWidth = 16, regWidth = 8, clkFreq = 100, coverage = false)
    info(s"Data Width: ${myParams.dataWidth}, Address Width: ${myParams.addrWidth}")
    info("--------------------------------")

    name match {
      // Basic clock test
      case "masterClock" =>
        it should "generate the correct clock frequency for master mode" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              clockTests.masterClock(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      case "dividerBasic" =>
        it should "verify the Divider-based I2C clock generation in a simple scenario" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              clockTests.dividerBasicCheck(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      case "dividerRandom" =>
        it should "verify random BAUD values do produce toggles" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              clockTests.dividerRandomCheck(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      // Basic receive tests from "receiveTests.scala"
      case "masterDataReceive" =>
        it should "correctly receive data packets in master mode" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              receiveTests.masterDataReceive(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      case "slaveDataReceive" =>
        it should "correctly receive data packets in slave mode" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              receiveTests.slaveDataReceive(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      // Arbitration from "arbitrationTests.scala"
      case "arbitrationLost" =>
        it should "handle multi-master arbitration lost" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              arbitrationTests.masterArbitrationLostTest(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      // Repeated starts from "repeatedStartTests.scala"
      case "repeatedStart" =>
        it should "perform repeated start without stop" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              repeatedStartTests.masterRepeatedStartTest(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      // Bus error from "busErrorTests.scala"
      case "busErrorMaster" =>
        it should "detect a bus error in master" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              busErrorTests.masterBusErrorTest(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      case "busErrorSlave" =>
        it should "detect a bus error in slave" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              busErrorTests.slaveBusErrorTest(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      // Slave collision from "slaveCollisionTests.scala"
      case "slaveCollision" =>
        it should "detect collision in slave" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              slaveCollisionTests.slaveCollisionTest(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      // Stop condition from "stopConditionTests.scala"
      case "masterStopCondition" =>
        it should "release lines on master stop" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              stopConditionTests.masterStopTest(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      case "slaveStopCondition" =>
        it should "release lines on slave stop" in {
          val cov = test(new I2C(myParams))
            .withAnnotations(backendAnnotations) { dut =>
              stopConditionTests.slaveStopTest(dut, myParams)
            }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      case "masterSlaveTransmission" =>
        it should "transmit data between master and slave" in {
          val cov = test(new FullDuplexI2C(myParams)).withAnnotations(backendAnnotations) { dut =>
            transmitTests.masterSlaveTransmission(dut, myParams)  
          }
          coverageCollection(cov.getAnnotationSeq, myParams, name)
        }

      // Default
      case _ =>
        runAllTests(myParams) // If unknown test name, run them all
    }
  }

  /** A convenience method to run all major tests. */
  def runAllTests(myParams: BaseParams): Unit = {
    // Put whichever test calls you want here
    clockTestFull(myParams)
    receiveTestsFull(myParams)
    advancedTestsFull(myParams)
  }

  /** Basic clock test. */
  def clockTestFull(myParams: BaseParams): Unit = {
    it should "generate the correct clock frequency for master mode" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        clockTests.masterClock(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "masterClock")
    }

    it should "verify the Divider-based I2C clock generation in a simple scenario" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        clockTests.dividerBasicCheck(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "dividerBasic")
    }

    it should "verify random BAUD values do produce toggles" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        clockTests.dividerRandomCheck(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "dividerRandom")
    }
  }

  /** Full receive tests. */
  def receiveTestsFull(myParams: BaseParams): Unit = {
    it should "correctly receive data packets in master mode" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        receiveTests.masterDataReceive(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "masterDataReceive")
    }

    it should "correctly receive data packets in slave mode" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        receiveTests.slaveDataReceive(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "slaveDataReceive")
    }
  }

  /** All advanced features: arbitration, repeated starts, etc. */
  def advancedTestsFull(myParams: BaseParams): Unit = {
    it should "handle multi-master arbitration lost" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        arbitrationTests.masterArbitrationLostTest(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "arbitrationLost")
    }

    it should "perform repeated start without stop" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        repeatedStartTests.masterRepeatedStartTest(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "repeatedStart")
    }

    it should "detect a bus error in master" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        busErrorTests.masterBusErrorTest(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "busErrorMaster")
    }

    it should "detect a bus error in slave" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        busErrorTests.slaveBusErrorTest(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "busErrorSlave")
    }

    it should "detect collision in slave" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        slaveCollisionTests.slaveCollisionTest(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "slaveCollision")
    }

    it should "release lines on master stop" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        stopConditionTests.masterStopTest(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "masterStopCondition")
    }

    it should "release lines on slave stop" in {
      val cov = test(new I2C(myParams)).withAnnotations(backendAnnotations) { dut =>
        stopConditionTests.slaveStopTest(dut, myParams)
      }
      coverageCollection(cov.getAnnotationSeq, myParams, "slaveStopCondition")
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
