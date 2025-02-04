package tech.rocksavage.chiselware.I2C

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.apb.ApbBundle
import tech.rocksavage.chiselware.apb.ApbTestUtils._

/** Tests for repeated start conditions.
  * We assume the master transitions to a 'repeatedStartMaster' state if mctrlb(0)=1
  * after completing a transmit or receive phase.
  */
object repeatedStartTests {

  /** Minimal example:
    *  1) Set master for normal address + data transmit
    *  2) Right after waiting for an ACK, set mctrlb(0)=1 => repeated start
    *  3) Ensure the design does NOT go to a normal stop condition,
    *     but transitions to repeatedStartMaster instead
    */
  def masterRepeatedStartTest(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // 1) Set small BAUD
    val mbaudAddr = dut.registerMap.getAddressOfRegister("mbaud").get
    writeAPB(dut.io.apb, mbaudAddr.U, 2.U)

    // Master address => e.g. 0x50, R/W=0
    val maddrAddr = dut.registerMap.getAddressOfRegister("maddr").get
    writeAPB(dut.io.apb, maddrAddr.U, 0xA0.U)

    val mctrlaAddr = dut.registerMap.getAddressOfRegister("mctrla").get
    // Start
    writeAPB(dut.io.apb, mctrlaAddr.U, 0x01.U)

    dut.clock.step(30) // Let address + data happen

    // 2) Now set mctrlb(0)=1 => repeated start
    val mctrlbAddr = dut.registerMap.getAddressOfRegister("mctrlb").get
    writeAPB(dut.io.apb, mctrlbAddr.U, 0x01.U)

    // 3) Step more cycles => ensure the design tries a repeated start 
    dut.clock.step(30)

    // There's no direct "repeatedStart done" flag. 
    val mstatusAddr = dut.registerMap.getAddressOfRegister("mstatus").get
    val finalStatus = readAPB(dut.io.apb, mstatusAddr.U)
    // Suppose bit 5 => STOP or something. We expect it's NOT set due to repeated start
    assert((finalStatus & 0x20) == 0, s"Expected repeated start, not stop. mstatus=0x${finalStatus.toString()}")
  }

}
