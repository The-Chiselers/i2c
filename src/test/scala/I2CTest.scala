// (c) 2024 Rocksavage Technology, Inc.
// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)
package tech.rocksavage.chiselware.i2c

import java.io.File
import chisel3._
import chisel3.tester._
import chiseltest._
import chiseltest.coverage._
import firrtl2.annotations.Annotation
import firrtl2.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// ########################
// I2C Test Harness
// ########################
class I2CTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val numTests = 2
  val testName = System.getProperty("testName")
  println(s"Running test: $testName")

  val enableVcd = System.getProperty("enableVcd", "false").toBoolean
  val enableFst = System.getProperty("enableFst", "true").toBoolean
  val useVerilator = System.getProperty("useVerilator", "false").toBoolean

  val buildRoot = sys.env.get("BUILD_ROOT_RELATIVE")
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
      annos = annos :+ VerilatorCFlags(Seq("--std=c++17"))
    }
    annos = annos :+ TargetDirAnnotation(testDir)

    annos
  }

  if (testName == "regression") {
    (1 to numTests).foreach { config =>
      runTest(s"I2C_test_config_$config")
    }
  } else {
    runTest(testName)
  }

  def runTest(testName: String): Unit = {
    behavior of testName

    val myParams = BaseParams(dataWidth = 8, addrWidth = 16, regWidth = 8)
    info(s"Data Width: ${myParams.dataWidth}, Address Width: ${myParams.addrWidth}")
    info("--------------------------------")

    testName match {
      case "masterClock" =>
        it should "generate the correct clock frequency for master mode" in {
          val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
            clockTests.masterClock(dut, myParams)
          }
          saveCoverage(cov.getAnnotationSeq, myParams, testName)
        }

      case "masterAddressTransmit" =>
        it should "correctly transmit address packets in master mode" in {
          val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
            transmitTests.masterAddressTransmit(dut, myParams)
          }
          saveCoverage(cov.getAnnotationSeq, myParams, testName)
        }

      case "masterDataTransmit" =>
        it should "correctly transmit data packets in master mode" in {
          val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
            transmitTests.masterDataTransmit(dut, myParams)
          }
          saveCoverage(cov.getAnnotationSeq, myParams, testName)
        }

      case "masterDataReceive" =>
        it should "correctly receive data packets in master mode" in {
          val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
            receiveTests.masterDataReceive(dut, myParams)
          }
          saveCoverage(cov.getAnnotationSeq, myParams, testName)
        }

      case "slaveAddressReceive" =>
        it should "correctly receive address packets in slave mode" in {
          val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
            receiveTests.slaveAddressReceive(dut, myParams)
          }
          saveCoverage(cov.getAnnotationSeq, myParams, testName)
        }

      case "slaveDataReceive" =>
        it should "correctly receive data packets in slave mode" in {
          val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
            receiveTests.slaveDataReceive(dut, myParams)
          }
          saveCoverage(cov.getAnnotationSeq, myParams, testName)
        }

      case "slaveDataTransmit" =>
        it should "correctly transmit data packets in slave mode" in {
          val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
            transmitTests.slaveDataTransmit(dut, myParams)
          }
          saveCoverage(cov.getAnnotationSeq, myParams, testName)
        }

      case "allTests" =>
        runAllTests(myParams)

      case _ =>
        runAllTests(myParams)
    }

    it should "generate cumulative coverage report" in {
      saveCumulativeCoverage(myParams)
    }
  }

  def runAllTests(myParams: BaseParams): Unit = {
    clockTestsFull(myParams)
    transmitTestsFull(myParams)
    receiveTestsFull(myParams)
  }

  def clockTestsFull(myParams: BaseParams): Unit = {
    it should "generate the correct clock frequency for master mode" in {
      val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
        clockTests.masterClock(dut, myParams)
      }
      saveCoverage(cov.getAnnotationSeq, myParams, "masterClock")
    }
  }

  def transmitTestsFull(myParams: BaseParams): Unit = {
    it should "correctly transmit address packets in master mode" in {
      val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
        transmitTests.masterAddressTransmit(dut, myParams)
      }
      saveCoverage(cov.getAnnotationSeq, myParams, "masterAddressTransmit")
    }

    it should "correctly transmit data packets in master mode" in {
      val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
        transmitTests.masterDataTransmit(dut, myParams)
      }
      saveCoverage(cov.getAnnotationSeq, myParams, "masterDataTransmit")
    }

    it should "correctly transmit data packets in slave mode" in {
      val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
        transmitTests.slaveDataTransmit(dut, myParams)
      }
      saveCoverage(cov.getAnnotationSeq, myParams, "slaveDataTransmit")
    }
  }

  def receiveTestsFull(myParams: BaseParams): Unit = {
    it should "correctly receive data packets in master mode" in {
      val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
        receiveTests.masterDataReceive(dut, myParams)
      }
      saveCoverage(cov.getAnnotationSeq, myParams, "masterDataReceive")
    }

    it should "correctly receive address packets in slave mode" in {
      val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
        receiveTests.slaveAddressReceive(dut, myParams)
      }
      saveCoverage(cov.getAnnotationSeq, myParams, "slaveAddressReceive")
    }

    it should "correctly receive data packets in slave mode" in {
      val cov = test(new I2C()).withAnnotations(backendAnnotations) { dut =>
        receiveTests.slaveDataReceive(dut, myParams)
      }
      saveCoverage(cov.getAnnotationSeq, myParams, "slaveDataReceive")
    }
  }

  def saveCoverage(cov: Seq[Annotation], myParams: BaseParams, testName: String): Unit = {
    val coverage = cov.collectFirst { case a: TestCoverage => a.counts }.get.toMap
    val testConfig = s"${myParams.addrWidth}_${myParams.dataWidth}"
    val coverageDir = new File(s"${buildRoot.get}/cov/scala")
    coverageDir.mkdirs()
    val coverageFile = new File(coverageDir, s"$testName_$testConfig.cov")
    checkCoverage(coverage, coverageFile)
  }

  def saveCumulativeCoverage(myParams: BaseParams): Unit = {
    val cumulativeCoverageDir = new File(s"${buildRoot.get}/cov/cumulative")
    cumulativeCoverageDir.mkdirs()
    val cumulativeFile = new File(cumulativeCoverageDir, "cumulative.cov")
    println(s"Cumulative coverage saved to $cumulativeFile")
  }
}
