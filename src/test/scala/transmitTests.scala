package tech.rocksavage.chiselware.I2C

import chisel3._
import chiseltest._
import scala.math.pow
import scala.util.Random
import tech.rocksavage.chiselware.apb.ApbBundle
import tech.rocksavage.chiselware.apb.ApbTestUtils._

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

  def masterAddressTransmit(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // 1) Write a small baud so each bit is ~2 cycles
    val mbaudAddr = dut.registerMap.getAddressOfRegister("mbaud").get
    writeAPB(dut.io.apb, mbaudAddr.U, 2.U)

    // 2) Set up address
    val slaveAddress = 0x50
    val writeFlag = 0
    val combinedAddress = (slaveAddress << 1) | writeFlag

    val maddrAddr = dut.registerMap.getAddressOfRegister("maddr").get
    writeAPB(dut.io.apb, maddrAddr.U, combinedAddress.U)

    // 3) Master enable + start
    val mctrlaAddr = dut.registerMap.getAddressOfRegister("mctrla").get
    // Suppose bit0 => start
    // bit7 => enable master, for instance
    writeAPB(dut.io.apb, mctrlaAddr.U, "b10000001".U)

    // 4) We expect SDA to go low for start eventually
    stepAndCheckSDA(dut, 20, expected=false, "Check start condition")

    // 5) Wait enough cycles for 8 bits + 1 ack => ~9 bits => 9 * 4 cycles => 36
    dut.clock.step(36)
    // We won't do a direct bit-by-bit check here, just ensure we see an ack in waitAckMasterAddr
    // The code in i2c.scala sets sdaOut=1 and reads sdaIn for ack, so sdaOut might be high if we do not see real slave ack.
    // For now, let's just assert no crash. 
  }

  def masterDataTransmit(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // 1) again set small baud
    val mbaudAddr = dut.registerMap.getAddressOfRegister("mbaud").get
    writeAPB(dut.io.apb, mbaudAddr.U, 2.U)

    // 2) set data
    val data = 0xA5
    val mdataAddr = dut.registerMap.getAddressOfRegister("mdata").get
    writeAPB(dut.io.apb, mdataAddr.U, data.U)

    // 3) Master enable + start
    val mctrlaAddr = dut.registerMap.getAddressOfRegister("mctrla").get
    writeAPB(dut.io.apb, mctrlaAddr.U, "b00000001".U)

    // 4) Wait for the 8 data bits + ack
    dut.clock.step(36)
    // Again, we skip a direct bit check; real tests would track SCL edges
  }

  def slaveAddressTransmit(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // We want the slave to transmit, but that normally requires the master to do R/W=1.
    // For a simpler demo, let's forcibly set the slave into dataTransmit state.
    val saddrAddr = dut.registerMap.getAddressOfRegister("saddr").get
    writeAPB(dut.io.apb, saddrAddr.U, 0x50.U)

    val sctrlaAddr = dut.registerMap.getAddressOfRegister("sctrla").get
    // bit0 => slave enable
    writeAPB(dut.io.apb, sctrlaAddr.U, "b00000001".U)

    // For real I2C, a master would do the start + read address (bit0=1).
    // We'll skip that part and just confirm the slave can shift something out if we force it.
    // This is not a fully correct test, but outlines the approach.
    dut.clock.step(10) // no actual check here
  }

  def slaveDataTransmit(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    val data = 0xA5
    val sdataAddr = dut.registerMap.getAddressOfRegister("sdata").get
    writeAPB(dut.io.apb, sdataAddr.U, data.U)

    val sctrlaAddr = dut.registerMap.getAddressOfRegister("sctrla").get
    writeAPB(dut.io.apb, sctrlaAddr.U, "b00000001".U) // Enable slave

    // In a real scenario, the slave waits for the master to read from it.
    // For demonstration, we forcibly put the slave FSM into dataTransmitSlave or rely on address + R/W=1 logic.
    dut.clock.step(20)
  }

    def masterSlaveTransmission(dut: FullDuplexI2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock
    dut.clock.setTimeout(0) 

    // 1) Setup R/W=1 => 0xA1
    val maddrReg  = dut.getMasterRegisterMap.getAddressOfRegister("maddr").get
    val mbaudReg  = dut.getMasterRegisterMap.getAddressOfRegister("mbaud").get
    val mctrlaReg = dut.getMasterRegisterMap.getAddressOfRegister("mctrla").get
    val mdataReg  = dut.getMasterRegisterMap.getAddressOfRegister("mdata").get
    
    val sctrlaReg = dut.getSlaveRegisterMap.getAddressOfRegister("sctrla").get
    val saddrReg  = dut.getSlaveRegisterMap.getAddressOfRegister("saddr").get

    val masterData = BigInt(params.dataWidth, Random)
    writeAPB(dut.io.slaveApb, sctrlaReg.U, 1.U)  // start
    writeAPB(dut.io.slaveApb, saddrReg.U, 0xa1.U)

    writeAPB(dut.io.masterApb, maddrReg.U, 0xa1.U)
    writeAPB(dut.io.masterApb, mdataReg.U, masterData.U)
    writeAPB(dut.io.masterApb, mbaudReg.U, 2.U)


    writeAPB(dut.io.masterApb, mctrlaReg.U, 1.U)  // start
    // Wait ~40 cycles for address phase
    dut.clock.step(1000)
  }

}
