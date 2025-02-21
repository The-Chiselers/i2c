package tech.rocksavage.chiselware.I2C

import chisel3._
import chiseltest._
import scala.math.pow
import scala.util.Random
import tech.rocksavage.chiselware.apb.ApbBundle
import tech.rocksavage.chiselware.apb.ApbTestUtils._
import org.scalatest.Assertions.fail

object transmitTests {

  // Helper: run "enough" steps until we see a stable moment in sdaOut.
  // Here we do a fixed cycle count for brevity.
  private def stepAndCheckSDA(
    dut: I2C,
    cycles: Int,
    expected: Boolean,
    msg: String = ""
  )(implicit clk: Clock): Unit = {
    var matched = false
    for (_ <- 0 until cycles) {
      dut.clock.step(1)
      //if (dut.io.master.sdaOut.peek().litToBoolean == expected) {
      //  matched = true
      //}
    }
    assert(matched, s"Never observed sdaOut=$expected within $cycles cycles. $msg")
  }
  
  /**
    * A demonstration test that configures a slave with address=0x50 and data=0x5A,
    * then configures the master to read from 0x50 (R/W=1 => 0xA1).
    *
    * Instead of stepping many cycles in one block, we step single cycles ~200-300 times
    * so that the master's internally generated SCL occurs in "real time," letting the FSM
    * shift bits properly.
    */
  def masterSlaveTransmission(dut: FullDuplexI2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock
    dut.clock.setTimeout(0)

    // --- Configure the Slave ---
    val sctrlaReg = dut.getSlaveRegisterMap.getAddressOfRegister("sctrla").get
    val saddrReg  = dut.getSlaveRegisterMap.getAddressOfRegister("saddr").get
    val sdataReg  = dut.getSlaveRegisterMap.getAddressOfRegister("sdata").get

    // Put some known data into the slave's SDATA register (0x5A).
    val slaveData = BigInt(params.dataWidth, Random)
    writeAPB(dut.io.slaveApb, sctrlaReg.U, 1.U)        // enable the slave
    writeAPB(dut.io.slaveApb, saddrReg.U, 0x50.U)      // slave address = 0x50
    writeAPB(dut.io.slaveApb, sdataReg.U, slaveData.U) // put 0x5A in SDATA

    // --- Configure the Master ---
    val maddrReg  = dut.getMasterRegisterMap.getAddressOfRegister("maddr").get
    val mbaudReg  = dut.getMasterRegisterMap.getAddressOfRegister("mbaud").get
    val mctrlReg = dut.getMasterRegisterMap.getAddressOfRegister("mctrl").get
    val mdataReg  = dut.getMasterRegisterMap.getAddressOfRegister("mdata").get

    // We'll do a read transaction => (slave address=0x50, R/W=1 => 0xA1)
    writeAPB(dut.io.masterApb, maddrReg.U, 0xA1.U)
    // The master's MDATA can be any dummy initially
    writeAPB(dut.io.masterApb, mdataReg.U, 0x00.U)
    // Set a small BAUD => For example 2 => halfPeriod ~ 7 cycles => full period ~14
    writeAPB(dut.io.masterApb, mbaudReg.U, 2.U)
    // Start the master
    writeAPB(dut.io.masterApb, mctrlReg.U, 1.U)

    // --- Step the clock "slowly" in single increments
    for(_ <- 0 until 800) {
      dut.clock.step(1)
    }

    // --- Read back the master's data register
    val masterReceived = readAPB(dut.io.masterApb, mdataReg.U).toInt
    // We expect it to match slaveData (0x5A)
    assert(
      masterReceived == slaveData.toInt,
      f"Master received 0x$masterReceived%02X, expected 0x$slaveData%02X"
    )
  }

  /**
    * Waits for one rising edge on master.scl.
    * Steps the system clock until the master.scl signal transitions from false to true.
    * Debug messages print the cycle count and signal value.
    * If no rising edge is detected within maxCycles, the test will fail.
    */
  def waitForRisingEdgeOnMasterSCL(dut: FullDuplexI2C, maxCycles: Int = 100)
                                  (implicit clk: Clock): Boolean = {
    var prevScl = dut.io.master.sclOut.peekBoolean()
    var cycles  = 0
    println(s"[DEBUG] (waitForRisingEdge) Initial master.scl: $prevScl")
    while (cycles < maxCycles) {
      dut.clock.step(1)
      val nowScl = dut.io.master.sclOut.peekBoolean()
      // Debug print every 50 cycles
      if (cycles % 50 == 0) {
        println(s"[DEBUG] (waitForRisingEdge) Cycle $cycles, master.scl: $nowScl")
      }
      if (!prevScl && nowScl) {
        println(s"[DEBUG] (waitForRisingEdge) Rising edge detected at cycle $cycles, master.scl: $nowScl")
        return true
      }
      prevScl = nowScl
      cycles += 1
    }
    return false
  }

  /**
    * Test function for Master Write → Slave Read in FullDuplexI2C.
    *
    * In this configuration (Option C with master in normal mode and slave in test mode),
    * the master submodule drives SCL internally while the slave does not drive SCL.
    * The slave's SCL input is wired to the master's SCL output.
    *
    * This test uses the helper waitForRisingEdgeOnMasterSCL to synchronize to the master_scl toggles.
    */
  def masterWriteSlaveReadFullDuplex(dut: FullDuplexI2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock
    dut.clock.setTimeout(0)

    // --- Configure SLAVE registers ---
    val sctrlaReg = dut.getSlaveRegisterMap.getAddressOfRegister("sctrla").get
    val saddrReg  = dut.getSlaveRegisterMap.getAddressOfRegister("saddr").get
    val sdataReg  = dut.getSlaveRegisterMap.getAddressOfRegister("sdata").get

    // println("[DEBUG] Configuring slave registers:")
    writeAPB(dut.io.slaveApb, sctrlaReg.U, 1.U)     // enable slave
    writeAPB(dut.io.slaveApb, saddrReg.U, 0x50.U)    // slave address = 0x50

    // --- Configure MASTER registers ---
    val maddrReg  = dut.getMasterRegisterMap.getAddressOfRegister("maddr").get
    val mbaudReg  = dut.getMasterRegisterMap.getAddressOfRegister("mbaud").get
    val mctrlReg = dut.getMasterRegisterMap.getAddressOfRegister("mctrl").get
    val mdataReg  = dut.getMasterRegisterMap.getAddressOfRegister("mdata").get

    val masterData = BigInt(params.dataWidth, Random)
    // println(s"[DEBUG] Configuring master registers: masterData = 0x${masterData.toHexString}")
    writeAPB(dut.io.masterApb, maddrReg.U, 0xA0.U)   // 0x50 + R/W=0
    writeAPB(dut.io.masterApb, mbaudReg.U, 2.U)      // small BAUD => ~7 cycles per half period
    writeAPB(dut.io.masterApb, mctrlReg.U, 1.U)     // enable master
    dut.clock.step(100)
    writeAPB(dut.io.masterApb, mdataReg.U, masterData.U)
    dut.clock.step(1)

    // --- Wait for rising edges on master.scl ---
    // Estimate the number of rising edges needed for the transaction.
    // For one byte write: address (8 bits) + ACK, data (8 bits) + ACK, plus STOP.
    // Waiting for ~30 rising edges should be more than sufficient.
    val edgesToWait = 50
    println(s"[DEBUG] Waiting for $edgesToWait rising edges on master.scl")
    var edge = 0
    while (edge < edgesToWait && waitForRisingEdgeOnMasterSCL(dut, maxCycles = 100)) {
      println(s"[DEBUG] Completed rising edge number $edge")
      edge += 1
    }

    // --- Read the slave's sdata register ---
    val gotData = readAPB(dut.io.slaveApb, sdataReg.U).toInt
    println(s"[DEBUG] Final: Slave sdata read = 0x${gotData.toString}, expected = 0x${masterData.toString}")
    assert(gotData == masterData.toInt, f"Slave read 0x$gotData%02X, expected 0x$masterData%02X")
  }

  /**
  * bidirectionalHalfDuplex
  *
  * This test performs a bidirectional (half-duplex) transaction.
  * In phase 1, the master writes data (0xAB) to the slave (slave reads it).
  * In phase 2, the slave writes a new data byte (0xCD) and the master reads it.
  *
  * This test reuses the ideas from the previously working tests:
  *   - It uses APB register accesses to configure master and slave.
  *   - It synchronizes with the internal master SCL using waitForRisingEdgeOnMasterSCL.
  */
def bidirectionalHalfDuplex(dut: FullDuplexI2C, params: BaseParams): Unit = {
  implicit val clk: Clock = dut.clock
  dut.clock.setTimeout(0)

  // ---------------------------
  // PHASE 1: Master Write → Slave Read
  // ---------------------------
  // Configure the slave registers for receiving:
  val sctrlaReg = dut.getSlaveRegisterMap.getAddressOfRegister("sctrla").get
  val saddrReg  = dut.getSlaveRegisterMap.getAddressOfRegister("saddr").get
  val sdataReg  = dut.getSlaveRegisterMap.getAddressOfRegister("sdata").get

  // Enable the slave and set its address to 0x50.
  writeAPB(dut.io.slaveApb, sctrlaReg.U, 1.U)
  writeAPB(dut.io.slaveApb, saddrReg.U, 0x50.U)

  // Configure the master for a write transaction:
  // (The transmitted address should be 0x50 with R/W=0 → 0xA0)
  val maddrReg  = dut.getMasterRegisterMap.getAddressOfRegister("maddr").get
  val mbaudReg  = dut.getMasterRegisterMap.getAddressOfRegister("mbaud").get
  val mctrlReg = dut.getMasterRegisterMap.getAddressOfRegister("mctrl").get
  val mdataReg  = dut.getMasterRegisterMap.getAddressOfRegister("mdata").get

  val masterData1 = BigInt(params.dataWidth, Random)
  // Write the address (0xA0) and then start the master.
  writeAPB(dut.io.masterApb, maddrReg.U, 0xA0.U)
  writeAPB(dut.io.masterApb, mbaudReg.U, 2.U)
  writeAPB(dut.io.masterApb, mctrlReg.U, 1.U)
  // Let the transaction begin; wait a short time.
  dut.clock.step(100)
  // Write the data to be transmitted.
  writeAPB(dut.io.masterApb, mdataReg.U, masterData1.U)
  dut.clock.step(1)
  
  // Wait for enough rising edges for the transaction to complete.
  val edgesToWait1 = 60
    var edge = 0
    while (edge < edgesToWait1 && waitForRisingEdgeOnMasterSCL(dut, maxCycles = 100)) {
      println(s"[DEBUG] Completed rising edge number $edge")
      edge += 1
    }

  
  // Read the slave's data register
  val slaveReceived = readAPB(dut.io.slaveApb, sdataReg.U).toInt
  println(s"[DEBUG] Phase 1: Slave received = 0x${slaveReceived.toHexString}, expected = 0x${masterData1.toString}")
  assert(slaveReceived == masterData1.toInt,
    f"Phase 1 failed: Slave received 0x$slaveReceived%02X, expected 0x$masterData1%02X")

  // ---------------------------
  // PHASE 2: Slave Write → Master Read
  // ---------------------------
  // Reconfigure the slave for transmitting:
  // (Keep the same slave address, but now preload its sdata register with new data)
  val slaveData2 = BigInt(params.dataWidth, Random)
  writeAPB(dut.io.slaveApb, sctrlaReg.U, 1.U)         // ensure slave is enabled
  writeAPB(dut.io.slaveApb, saddrReg.U, 0x50.U)         // same slave address
  writeAPB(dut.io.slaveApb, sdataReg.U, slaveData2.U)   // preload slave data for transmission

  // Reconfigure the master for a read transaction:
  // For a read, the address should be (0x50 with R/W=1) → 0xA1.
  writeAPB(dut.io.masterApb, maddrReg.U, 0xA1.U)
  // Clear master's mdata (dummy initial value)
  writeAPB(dut.io.masterApb, mdataReg.U, 0x00.U)
  // Ensure BAUD is still 2.
  writeAPB(dut.io.masterApb, mbaudReg.U, 2.U)
  // Start the master for the read transaction.
  writeAPB(dut.io.masterApb, mctrlReg.U, 1.U)
  
  // Wait for the read transaction to complete by waiting for rising edges.
  val edgesToWait2 = 60
  var edge2 = 0
  while (edge2 < edgesToWait2 && waitForRisingEdgeOnMasterSCL(dut, maxCycles = 100)) {
    println(s"[DEBUG] Completed rising edge number $edge")
    edge2 += 1
  }

  
  // Read the master's data register
  val masterReceived = readAPB(dut.io.masterApb, mdataReg.U).toInt
  println(s"[DEBUG] Phase 2: Master received = 0x${masterReceived.toHexString}, expected = 0x${slaveData2.toString}")
  assert(masterReceived == slaveData2.toInt,
    f"Phase 2 failed: Master received 0x$masterReceived%02X, expected 0x$slaveData2%02X")
}

  /**
    * ackVsNackFullDuplex
    *
    * This test verifies that if the slave does not match the master’s address
    * (or is forced to NACK), then the master sees a NACK (RXACK=1 in MSTATUS).
    */
  def ackVsNackFullDuplex(dut: FullDuplexI2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock
    dut.clock.setTimeout(0)

    // --- Configure the SLAVE with a mismatched address ---
    val sctrlaReg = dut.getSlaveRegisterMap.getAddressOfRegister("sctrla").get
    val saddrReg  = dut.getSlaveRegisterMap.getAddressOfRegister("saddr").get
    println("[DEBUG] Setting slave address to 0x51 (mismatch)")
    writeAPB(dut.io.slaveApb, sctrlaReg.U, 1.U)
    writeAPB(dut.io.slaveApb, saddrReg.U, 0x51.U) // mismatch with master's target 0x50

    // --- Configure the MASTER for a write (R/W=0 => 0xA0) ---
    val maddrReg   = dut.getMasterRegisterMap.getAddressOfRegister("maddr").get
    val mbaudReg   = dut.getMasterRegisterMap.getAddressOfRegister("mbaud").get
    val mctrlReg  = dut.getMasterRegisterMap.getAddressOfRegister("mctrl").get
    val mstatusReg = dut.getMasterRegisterMap.getAddressOfRegister("mstatus").get

    val addressByte = 0xA0 // 0x50 with R/W=0
    println(s"[DEBUG] Master address byte = 0x${addressByte.toHexString}")
    writeAPB(dut.io.masterApb, maddrReg.U, addressByte.U)
    writeAPB(dut.io.masterApb, mbaudReg.U, 2.U)
    writeAPB(dut.io.masterApb, mctrlReg.U, 1.U) // start the transaction

    // Wait for enough rising edges so the transaction can proceed.
    val edgesToWait = 30
    var edge = 0
    while (edge < edgesToWait && waitForRisingEdgeOnMasterSCL(dut, maxCycles = 100)) {
      println(s"[DEBUG] Completed rising edge number $edge")
      edge += 1
    }


    // --- Read master status and check RXACK (bit3 of mstatus) ---
  val mstat = readAPB(dut.io.masterApb, mstatusReg.U).toInt
  val rxack = (mstat >> 4) & 1  // Shift by 3 to get bit4 (RXACK)
  println(s"[DEBUG] Master MSTATUS = 0x${mstat.toHexString}, RXACK = $rxack")
  assert(rxack == 1, f"Expected NACK but got ACK (MSTATUS=0x$mstat%02X)")
  }
  
  /**
    * stopConditionFullDuplex
    *
    * In this test the master starts a short transaction and then issues a STOP command.
    * The slave should detect the STOP condition by updating its sstatus register,
    * for example by setting the APIF flag (assumed bit6 = 1) and clearing the AP bit (bit0 = 0).
    */
  def stopConditionFullDuplex(dut: FullDuplexI2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock
    dut.clock.setTimeout(0)

    // --- Configure the SLAVE with Stop Interrupt Enable (if implemented) ---
    // For example, assume bit2 (0x04) is PIEN and bit0 (0x01) is enable.
    val sctrlaReg = dut.getSlaveRegisterMap.getAddressOfRegister("sctrla").get
    val sstatusReg = dut.getSlaveRegisterMap.getAddressOfRegister("sstatus").get
    val pienMask = 0x04
    println(s"[DEBUG] Configuring slave with PIEN enabled (mask=0x${pienMask.toHexString})")
    writeAPB(dut.io.slaveApb, sctrlaReg.U, (pienMask | 0x01).U)

    // --- Configure the MASTER for a write (R/W=0 => 0xA0) ---
    val maddrReg  = dut.getMasterRegisterMap.getAddressOfRegister("maddr").get
    val mbaudReg  = dut.getMasterRegisterMap.getAddressOfRegister("mbaud").get
    val mctrlReg = dut.getMasterRegisterMap.getAddressOfRegister("mctrl").get
    // val mctrlbReg = dut.getMasterRegisterMap.getAddressOfRegister("mctrlb").get
    val mstatusReg = dut.getMasterRegisterMap.getAddressOfRegister("mstatus").get

    println("[DEBUG] Configuring master for a write transaction")
    writeAPB(dut.io.masterApb, maddrReg.U, 0xA0.U)
    writeAPB(dut.io.masterApb, mbaudReg.U, 2.U)
    writeAPB(dut.io.masterApb, mctrlReg.U, 1.U)

    // Wait for a number of rising edges for the transaction to progress.
    val edgesToWait = 30
     var edge = 0
    while (edge < edgesToWait && waitForRisingEdgeOnMasterSCL(dut, maxCycles = 100)) {
      println(s"[DEBUG] Completed rising edge number $edge")
      edge += 1
    }


    // // Now issue a STOP command from the master.
    // val stopCmd = 0x03  // Assume writing 0x03 to MCTRLb issues a STOP.
    // println(s"[DEBUG] Issuing STOP command (0x03) to master")
    // writeAPB(dut.io.masterApb, mctrlbReg.U, stopCmd.U)

    // Wait (poll) until the slave's sstatus reflects a STOP condition.
    // That is, wait until sstatus has APIF = 1 (assumed bit6) and AP = 0 (assumed bit0).
    val timeout = 100
    var cycles = 0
    var stopDetected = false
    while (cycles < timeout && !stopDetected) {
      val sstat = readAPB(dut.io.slaveApb, sstatusReg.U).toInt
      val apif = (sstat >> 6) & 1
      val ap = sstat & 1
      if (apif == 1 && ap == 0) {
        stopDetected = true
      } else {
        dut.clock.step(1)
        cycles += 1
      }
    }
    println(s"[DEBUG] Final slave sstatus = 0x${readAPB(dut.io.slaveApb, sstatusReg.U).toString}")
    assert(stopDetected, s"Slave did not detect STOP. sstatus=0x${readAPB(dut.io.slaveApb, sstatusReg.U).toString}")
  }

  /**
    * noSlavePresentFullDuplex
    *
    * In this test the slave is disabled so that when the master sends its address,
    * no ACK is received. The master’s MSTATUS should then indicate a NACK.
    */
  def noSlavePresentFullDuplex(dut: FullDuplexI2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock
    dut.clock.setTimeout(0)

    // Disable the slave by writing 0 to sctrla.
    val sctrlaReg = dut.getSlaveRegisterMap.getAddressOfRegister("sctrla").get
    println("[DEBUG] Disabling slave (sctrla = 0)")
    writeAPB(dut.io.slaveApb, sctrlaReg.U, 0.U)

    // --- Configure the MASTER for a write transaction ---
    val maddrReg   = dut.getMasterRegisterMap.getAddressOfRegister("maddr").get
    val mbaudReg   = dut.getMasterRegisterMap.getAddressOfRegister("mbaud").get
    val mctrlReg  = dut.getMasterRegisterMap.getAddressOfRegister("mctrl").get
    val mstatusReg = dut.getMasterRegisterMap.getAddressOfRegister("mstatus").get

    println("[DEBUG] Configuring master for a write transaction (no slave present)")
    writeAPB(dut.io.masterApb, maddrReg.U, 0xA0.U)
    writeAPB(dut.io.masterApb, mbaudReg.U, 2.U)
    writeAPB(dut.io.masterApb, mctrlReg.U, 1.U)

    // Wait for enough rising edges for the master transaction to complete.
    val edgesToWait = 30
    var edge = 0
    while (edge < edgesToWait && waitForRisingEdgeOnMasterSCL(dut, maxCycles = 100)) {
      println(s"[DEBUG] Completed rising edge number $edge")
      edge += 1
    }


    // Read master status and check that RXACK is set (assumed to be bit4)
    val mstat = readAPB(dut.io.masterApb, mstatusReg.U).toInt
    val rxack = (mstat >> 4) & 1
    println(s"[DEBUG] Master MSTATUS = 0x${mstat.toHexString}, RXACK = $rxack")
    assert(rxack == 1, f"Expected NACK (rxack=1) but got MSTATUS=0x$mstat%02X")
  }

}