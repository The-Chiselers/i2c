package tech.rocksavage.chiselware.I2C

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.apb.ApbBundle
import tech.rocksavage.chiselware.apb.ApbTestUtils._

/** Tests for multi-master arbitration lost scenarios.
  * In a real test, you'd need two "masters" contending on the bus. 
  * Here, we force the bus to go low externally while the design tries to let it go high.
  */
object arbitrationTests {

   /**
    * Waits for one rising edge on master.scl.
    * Steps the system clock until the master.scl signal transitions from false to true.
    * Debug messages print the cycle count and signal value.
    * If no rising edge is detected within maxCycles, the test will fail.
    */
  def waitForRisingEdgeOnMasterSCL(dut: MultiMasterI2C, maxCycles: Int = 1000)
                                  (implicit clk: Clock): Unit = {
    var prevScl = dut.io.master1.sclOut.peekBoolean() & dut.io.master2.sclOut.peekBoolean()
    var cycles  = 0
    println(s"[DEBUG] (waitForRisingEdge) Initial master.scl: $prevScl")
    while (cycles < maxCycles) {
      dut.clock.step(1)
      val nowScl =  dut.io.master1.sclOut.peekBoolean() & dut.io.master2.sclOut.peekBoolean()
      // Debug print every 50 cycles
      if (cycles % 50 == 0) {
        println(s"[DEBUG] (waitForRisingEdge) Cycle $cycles, master.scl: $nowScl")
      }
      if (!prevScl && nowScl) {
        println(s"[DEBUG] (waitForRisingEdge) Rising edge detected at cycle $cycles, master.scl: $nowScl")
        return
      }
      prevScl = nowScl
      cycles += 1
    }
    // If no rising edge is detected, fail the test.
    assert(false, s"Timed out waiting for rising edge on master.scl after $cycles cycles")
  }

  /** Minimal example:
    *  1) Set master mode, enable start
    *  2) Force a collision on SDA (the master tries to let SDA=1, but we pull it low)
    *  3) Wait enough cycles to let the design detect ARB lost
    *  4) Check mstatus for ARBLOST bit
    */
 def multiMasterWriteSlaveRead(dut: MultiMasterI2C, params: BaseParams): Unit = {
  implicit val clk: Clock = dut.clock
  dut.clock.setTimeout(0)

  // --- Configure SLAVE registers ---
  val sctrlaReg = dut.getSlaveRegisterMap.getAddressOfRegister("sctrla").get
  val saddrReg  = dut.getSlaveRegisterMap.getAddressOfRegister("saddr").get
  val sdataReg  = dut.getSlaveRegisterMap.getAddressOfRegister("sdata").get

  // Enable slave and set slave address
  writeAPB(dut.io.slaveApb, sctrlaReg.U, 1.U)     // enable slave
  writeAPB(dut.io.slaveApb, saddrReg.U, 0x50.U)  // slave address = 0x50

  // --- Configure MASTER 1 and MASTER 2 registers concurrently ---
  val maddrReg1  = dut.getMasterRegisterMap1.getAddressOfRegister("maddr").get
  val mbaudReg1  = dut.getMasterRegisterMap1.getAddressOfRegister("mbaud").get
  val mctrlReg1 = dut.getMasterRegisterMap1.getAddressOfRegister("mctrl").get
  val mdataReg1  = dut.getMasterRegisterMap1.getAddressOfRegister("mdata").get
  val mstatusReg1 = dut.getMasterRegisterMap1.getAddressOfRegister("mstatus").get

  val maddrReg2  = dut.getMasterRegisterMap2.getAddressOfRegister("maddr").get
  val mbaudReg2  = dut.getMasterRegisterMap2.getAddressOfRegister("mbaud").get
  val mctrlReg2 = dut.getMasterRegisterMap2.getAddressOfRegister("mctrl").get
  val mdataReg2  = dut.getMasterRegisterMap2.getAddressOfRegister("mdata").get
  val mstatusReg2 = dut.getMasterRegisterMap2.getAddressOfRegister("mstatus").get

  val master1Data = 0xAB
  val master2Data = 0xCD

  // Fork to write maddr, mbaud, and mctrl registers concurrently for both masters
  fork {
    // Master 1 configuration
    writeAPB(dut.io.masterApb1, maddrReg1.U, 0xA0.U)   // 0x50 + R/W=0
    writeAPB(dut.io.masterApb1, mbaudReg1.U, 2.U)      // small BAUD => ~7 cycles per half period
    writeAPB(dut.io.masterApb1, mctrlReg1.U, 1.U)     // enable master
  }.fork {
    // Master 2 configuration
    writeAPB(dut.io.masterApb2, maddrReg2.U, 0xA0.U)   // 0x50 + R/W=0
    writeAPB(dut.io.masterApb2, mbaudReg2.U, 2.U)      // small BAUD => ~7 cycles per half period
    writeAPB(dut.io.masterApb2, mctrlReg2.U, 1.U)     // enable master
  }.join()  // Wait for both forks to complete

  // Step the clock to allow the writes to take effect
  dut.clock.step(100)

  // Fork to write mdata registers concurrently for both masters
  fork {
    // Master 1 data write
    writeAPB(dut.io.masterApb1, mdataReg1.U, master1Data.U)
  }.fork {
    // Master 2 data write
    writeAPB(dut.io.masterApb2, mdataReg2.U, master2Data.U)
  }.join()  // Wait for both forks to complete

  // Step the clock to allow the data writes to take effect
  dut.clock.step(1)

  // --- Wait for rising edges on SCL ---
  // Estimate the number of rising edges needed for the transaction.
  // For one byte write: address (8 bits) + ACK, data (8 bits) + ACK, plus STOP.
  // Waiting for ~30 rising edges should be more than sufficient.
  val edgesToWait = 15
  println(s"[DEBUG] Waiting for $edgesToWait rising edges on SCL")
  for (edge <- 0 until edgesToWait) {
    waitForRisingEdgeOnMasterSCL(dut, maxCycles = 1000)
    println(s"[DEBUG] Completed rising edge number $edge")
  }

  // --- Read the slave's sdata register ---
  val gotData = readAPB(dut.io.slaveApb, sdataReg.U).toInt
  println(s"[DEBUG] Final: Slave sdata read = 0x${gotData.toHexString}")

  // --- Check arbitration results ---
  val master1Status = readAPB(dut.io.masterApb1, mstatusReg1.U).toInt
  val master2Status = readAPB(dut.io.masterApb2, mstatusReg2.U).toInt

  println(s"[DEBUG] Master 1 status = 0x${master1Status.toHexString}")
  println(s"[DEBUG] Master 2 status = 0x${master2Status.toHexString}")

  // Check which master lost arbitration
  if ((master1Status & 0x4) != 0) {
    println("[DEBUG] Master 1 lost arbitration")
    assert((master1Status & 0x3) == 2, "Master 1 status(1:0) should be 10 (arbitration lost)")
  } else if ((master2Status & 0x4) != 0) {
    println("[DEBUG] Master 2 lost arbitration")
    assert((master2Status & 0x3) == 2, "Master 2 status(1:0) should be 10 (arbitration lost)")
  } else {
    assert(false, "Neither master lost arbitration, which is unexpected")
  }

  // Check which master won arbitration and verify the data
  if ((master1Status & 0x4) == 0) {
    println("[DEBUG] Master 1 won arbitration")
    assert(gotData == master1Data, f"Slave read 0x$gotData%02X, expected 0x$master1Data%02X")
  } else if ((master2Status & 0x4) == 0) {
    println("[DEBUG] Master 2 won arbitration")
    assert(gotData == master2Data, f"Slave read 0x$gotData%02X, expected 0x$master2Data%02X")
  }
}

}
