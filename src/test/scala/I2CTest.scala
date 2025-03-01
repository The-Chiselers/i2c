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
import chiseltest.simulator.VerilatorCFlags
import chiseltest.formal.BoundedCheck
import chiseltest.RawTester.verify
import firrtl2.AnnotationSeq
import firrtl2.annotations.Annotation
import firrtl2.options.TargetDirAnnotation

import tech.rocksavage.test._

class I2CTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers
    with ApbUtils {

  val numTests     = 2
  var testName     = System.getProperty("testName")
  if (testName == null) {
    testName = "all"
  }
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
      annos = annos :+ VerilatorCFlags(Seq("--std=c++17"))
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
    val validDataWidths = Seq(8, 16, 32)
    val dataWidth = validDataWidths(Random.nextInt(validDataWidths.length))
    // Example I2C parameters
    val myParams = BaseParams(dataWidth, addrWidth = 16, regWidth = 8, clkFreq = 50, coverage = true)
    info(s"Data Width: ${myParams.dataWidth}, Address Width: ${myParams.addrWidth}")
    info("--------------------------------")
    val covDir   = "./out/cov"
    val coverage = true
    val configName = dataWidth + "_" + "_16" + "_8"

    name match {
      // Basic clock test

      case "formal_mm" =>
        "MultiMasterI2C" should "Formally Verify" in
          verify(
            new MultiMasterI2C(myParams, true),
            Seq(BoundedCheck(40)),
          )
      case "formal_fullduplex" =>
        "FullDuplexI2C" should "Formally Verify" in
          verify(
            new FullDuplexI2C(myParams, true),
            Seq(BoundedCheck(40)),
          )

      case "masterClock" =>
        it should "generate the correct clock frequency for master mode" in {
          val cov = test(new FullDuplexI2C((myParams)))
            .withAnnotations(backendAnnotations) { dut =>
              clockTests.masterClock(dut, myParams)
            }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )        }

      case "dividerBasic" =>
        it should "verify the Divider-based I2C clock generation in a simple scenario" in {
          val cov = test(new FullDuplexI2C((myParams)))
            .withAnnotations(backendAnnotations) { dut =>
              clockTests.dividerBasicCheck(dut, myParams)
            }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )        
          }

      case "dividerRandom" =>
        it should "verify random BAUD values do produce toggles" in {
          val cov = test(new FullDuplexI2C((myParams)))
            .withAnnotations(backendAnnotations) { dut =>
              clockTests.dividerRandomCheck(dut, myParams)
            }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }

      case "clockTests" =>
        clockTestFull(myParams, configName, covDir, coverage)

      // Arbitration from "arbitrationTests.scala"
      case "arbitrationLost" =>
        it should "handle multi-master arbitration lost" in {
          val cov = test(new MultiMasterI2C((myParams)))
            .withAnnotations(backendAnnotations) { dut =>
              arbitrationTests.multiMasterWriteSlaveRead(dut, myParams)
            }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }

        case "multiMasterExtended" =>
        it should "handle multi-master arbitration and extra transmission checks" in {
          val cov = test(new MultiMasterI2C((myParams)))
            .withAnnotations(backendAnnotations) { dut =>
              arbitrationTests.multiMasterExtened(dut, myParams)
            }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }


      // Repeated starts from "repeatedStartTests.scala"
      case "repeatedStart" =>
        it should "perform repeated start without stop" in {
          val cov = test(new I2C((myParams)))
            .withAnnotations(backendAnnotations) { dut =>
              repeatedStartTests.masterRepeatedStartTest(dut, myParams)
            }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }

      // For full-duplex test, use the master portion of the FullDuplexI2C.
      case "masterSlaveTransmission" =>
        it should "transmit data between master and slave" in {
          val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
            transmitTests.masterSlaveTransmission(dut, myParams)  
          }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }

      case "masterWriteSlaveReadFullDuplex" =>
        it should "perform Master Write->Slave Read in FullDuplexI2C" in {
          val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
            transmitTests.masterWriteSlaveReadFullDuplex(dut, myParams)
          }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }

      case "bidirectionalHalfDuplex" =>
        it should "perform bidirectional half-duplex communication" in {
          val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
            transmitTests.bidirectionalHalfDuplex(dut, myParams)
          }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }

      case "bidirectionalHalfDuplexTwice" =>
        it should "perform bidirectional half-duplex communication twice" in {
          val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
            transmitTests.bidirectionalHalfDuplexTwice(dut, myParams)
          }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }     

      case "ackVsNackFullDuplex" =>
        it should "check scenario for ack vs. NACK in FullDuplexI2C" in {
          val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
            transmitTests.ackVsNackFullDuplex(dut, myParams)
          }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }

      case "stopConditionFullDuplex" =>
        it should "verify Stop condition logic from Master->Slave in FullDuplexI2C" in {
          val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
            transmitTests.stopConditionFullDuplex(dut, myParams)
          }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }

      case "noSlavePresentFullDuplex" =>
        it should "check Master sees NACK if no slave present in FullDuplexI2C" in {
          val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
            transmitTests.noSlavePresentFullDuplex(dut, myParams)
          }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }

      case "clockStretchingSlave" =>
        it should "check if Master sees NACK & ACK during Clock Stretching" in {
          val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
            transmitTests.clockStretchingSlave(dut, myParams)
          }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }

      case "clockStretchingMaster" =>
        it should "check if Slave sees NACK & ACK during Clock Stretching" in {
          val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
            transmitTests.clockStretchingMaster(dut, myParams)
          }
            coverageCollector.collectCoverage(
              cov.getAnnotationSeq,
              testName,
              configName,
              coverage,
              covDir
            )
        }
      // default => runAllTests or handle other singled-out testName
      case _ =>
        runAllTests(myParams, configName, covDir, coverage)
    }
    
    it should "generate cumulative coverage report" in {
      coverageCollector.saveCumulativeCoverage(coverage, covDir)
    }
  }

  /** A convenience method to run all major tests. */
  def runAllTests(myParams: BaseParams, 
  configName: String,
  covDir: String,
  coverage: Boolean
  ): Unit = {
    clockTestFull(myParams, configName, covDir, coverage)
    advancedTestsFull(myParams, configName, covDir, coverage)
    fullDuplexTestsFull(myParams, configName, covDir, coverage)
  }

  // A new group for "FullDuplexI2C" tests:
  def fullDuplexTestsFull(myParams: BaseParams,
  configName: String,
  covDir: String,
  coverage: Boolean): Unit = {
    it should "transmit data (Master Read->Slave Write) between master and slave" in {
      val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        transmitTests.masterSlaveTransmission(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "masterSlaveTransmission",
        configName,
        coverage,
        covDir
      )    }

    it should "perform Master Write->Slave Read in FullDuplexI2C" in {
      val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        transmitTests.masterWriteSlaveReadFullDuplex(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "masterWritesSlaveReadFullDuplex",
        configName,
        coverage,
        covDir
      )
    }

    it should "perform bidirectional half-duplex communication" in {
      val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        transmitTests.bidirectionalHalfDuplex(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "bidirectionalHalfDuplex",
        configName,
        coverage,
        covDir
      )
    }

    it should "perform bidirectional half-duplex communication twice" in {
          val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
            transmitTests.bidirectionalHalfDuplexTwice(dut, myParams)
          }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "bidirectionalHalfDuplexTwice",
        configName,
        coverage,
        covDir
      )        
    }

    it should "check scenario for ack vs. NACK in FullDuplexI2C" in {
      val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        transmitTests.ackVsNackFullDuplex(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "ackVsNackFullDuplex",
        configName,
        coverage,
        covDir
      )    
    }

    it should "verify Stop condition logic from Master->Slave in FullDuplexI2C" in {
      val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        transmitTests.stopConditionFullDuplex(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "stopConditionFullDuplex",
        configName,
        coverage,
        covDir
      )    
    }

    it should "check Master sees NACK if no slave present in FullDuplexI2C" in {
      val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        transmitTests.noSlavePresentFullDuplex(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "noSlavePresentFullDuplex",
        configName,
        coverage,
        covDir
      )  
    }

    it should "check if Master sees NACK & ACK during Clock Stretching" in {
      val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        transmitTests.clockStretchingSlave(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "clockStretchingSlave",
        configName,
        coverage,
        covDir
      )      
    }
    it should "check if Slave sees NACK & ACK during Clock Stretching" in {
      val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        transmitTests.clockStretchingMaster(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "clockStretchingMaster",
        configName,
        coverage,
        covDir
      )      
    
    }
  }

  /** Basic clock tests. */
  def clockTestFull(myParams: BaseParams,
  configName: String,
  covDir: String,
  coverage: Boolean
  ): Unit = {
    it should "generate the correct clock frequency for master mode" in {
      val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        clockTests.masterClock(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "masterClock",
        configName,
        coverage,
        covDir
      )      
    }

    it should "verify the Divider-based I2C clock generation in a simple scenario" in {
      val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        clockTests.dividerBasicCheck(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "dividerBasic",
        configName,
        coverage,
        covDir
      )      
    }


    it should "verify random BAUD values do produce toggles" in {
      val cov = test(new FullDuplexI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        clockTests.dividerRandomCheck(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "dividerRandom",
        configName,
        coverage,
        covDir
      )      
    }
  }


  /** Advanced tests: arbitration, repeated starts, bus errors, collisions, stops. */
  def advancedTestsFull(myParams: BaseParams,
  configName: String,
  covDir: String,
  coverage: Boolean
  ): Unit = {
    it should "handle multi-master arbitration lost" in {
      val cov = test(new MultiMasterI2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        arbitrationTests.multiMasterWriteSlaveRead(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "arbitrationLost",
        configName,
        coverage,
        covDir
      )      
    }

    it should "handle multi-master arbitration and extra transmission checks" in {
    val cov = test(new MultiMasterI2C((myParams)))
      .withAnnotations(backendAnnotations) { dut =>
        arbitrationTests.multiMasterExtened(dut, myParams)
      }
      coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "multiMasterExtended",
        configName,
        coverage,
        covDir
      )
    }

    it should "perform repeated start without stop" in {
      val cov = test(new I2C((myParams))).withAnnotations(backendAnnotations) { dut =>
        repeatedStartTests.masterRepeatedStartTest(dut, myParams)
      }
    coverageCollector.collectCoverage(
        cov.getAnnotationSeq,
        "repeatedStart",
        configName,
        coverage,
        covDir
      )      
    }
  }
}
