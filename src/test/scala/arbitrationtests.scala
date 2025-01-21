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

  /** Minimal example:
    *  1) Set master mode, enable start
    *  2) Force a collision on SDA (the master tries to let SDA=1, but we pull it low)
    *  3) Wait enough cycles to let the design detect ARB lost
    *  4) Check mstatus for ARBLOST bit
    */
  def masterArbitrationLostTest(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // 1) Set small BAUD for quick toggles
    val mbaudAddr = dut.registerMap.getAddressOfRegister("mbaud").get
    writeAPB(dut.io.apb, mbaudAddr.U, 2.U)

    // 2) Master enable + start
    val mctrlaAddr = dut.registerMap.getAddressOfRegister("mctrla").get
    writeAPB(dut.io.apb, mctrlaAddr.U, 0x01.U)

    // Step a bit to let the start condition proceed
    dut.clock.step(10)

    // 3) Force collision: The master might want SDA high. 
    // We'll poke sdaIn= false => effectively pulling bus low externally. 
    // This is naive: in a real multi-master test, you'd have a second master driving the line.
    dut.io.master.sdaIn.poke(false.B)

    // Wait enough cycles for design to detect ARB lost
    dut.clock.step(20)

    // 4) Check ARBLOST bit in mstatus (assume bit6 = ARBLOST)
    val mstatusAddr = dut.registerMap.getAddressOfRegister("mstatus").get
    val mstatusVal  = readAPB(dut.io.apb, mstatusAddr.U)
    // If we expect bit 6 => 0x40 set
    assert((mstatusVal & 0x40) == 0x40, s"Arbitration not lost as expected. mstatus=0x${mstatusVal.toString()}")
  }

}
