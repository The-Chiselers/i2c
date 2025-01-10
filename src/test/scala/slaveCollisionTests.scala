package tech.rocksavage.chiselware.I2C

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.apb.ApbBundle
import tech.rocksavage.chiselware.apb.ApbTestUtils._

/** Tests for slave collision detection:
  * e.g. the slave tries to drive SDA=0, but the bus is forcibly 1
  */
object slaveCollisionTests {

  /** Minimal example:
    *  1) Enable slave, put it in dataTransmitSlave 
    *  2) We forcibly drive bus SDA high => sdaIn=true
    *  3) The slave tries to drive it low => collision
    *  4) Check sstatus for collision bit
    */
  def slaveCollisionTest(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // 1) enable slave
    val sctrlaAddr = dut.registerMap.getAddressOfRegister("sctrla").get
    writeAPB(dut.io.apb, sctrlaAddr.U, 0x01.U)

    // Suppose we also forcibly set the slave FSM to dataTransmit by faking an address match
    // or we can do a partial approach. This is "cheating" a bit but demonstrates collision.
    dut.clock.step(5)

    // 2) Force bus SDA high from the outside
    // (wired-AND => if we poke sdaIn= true, and the slave tries to drive false => collision)
    dut.io.sdaIn.poke(true.B)

    // 3) On the next falling edge of sclIn, the slave tries to drive sdaSlaveReg= false => collision
    // For demonstration, let's pulse sclIn low
    dut.io.sclIn.poke(false.B)
    dut.clock.step(2)
    dut.io.sclIn.poke(true.B)
    dut.clock.step(2)

    // 4) read sstatus => collision bit
    val sstatusAddr = dut.registerMap.getAddressOfRegister("sstatus").get
    val finalStat = readAPB(dut.io.apb, sstatusAddr.U)
    // Suppose bit5 => COLL
    assert((finalStat & 0x20) == 0x20, s"Expected slave collision. sstatus=0x${finalStat.toString()}")
  }

}
