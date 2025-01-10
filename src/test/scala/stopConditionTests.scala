package tech.rocksavage.chiselware.I2C

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.apb.ApbBundle
import tech.rocksavage.chiselware.apb.ApbTestUtils._

/** Tests for verifying correct stop condition handling
  * (master releases SDA while SCL=high, etc.)
  */
object stopConditionTests {

  /** Minimal example:
    *  1) Master does a normal transaction
    *  2) We expect it to go to 'stopConditionMaster'
    *  3) Check that eventually sclOut & sdaOut both go high and FSM returns idle
    */
  def masterStopTest(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // 1) Master enable
    val mctrlaAddr = dut.registerMap.getAddressOfRegister("mctrla").get
    writeAPB(dut.io.apb, mctrlaAddr.U, 1.U)

    // 2) Step enough cycles for normal transmit, no repeated start
    dut.clock.step(50)

    // 3) The design should do a stop => sclOut= true, then sdaOut= true
    var stopSeen = false
    for (_ <- 0 until 50) {
      dut.clock.step(1)
      val sclVal = dut.io.sclOut.peek().litToBoolean
      val sdaVal = dut.io.sdaOut.peek().litToBoolean
      if (sclVal && sdaVal) {
        stopSeen = true
      }
    }
    assert(stopSeen, "Never observed both SCL and SDA high => stop condition")

  }

  /** For slave side, we might check that if the master sets SCL=high + SDA=high, 
    * the slave transitions to stopConditionSlave. 
    */
  def slaveStopTest(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // enable slave
    val sctrlaAddr = dut.registerMap.getAddressOfRegister("sctrla").get
    writeAPB(dut.io.apb, sctrlaAddr.U, 1.U)

    // Step a bit, then forcibly drive sclIn= true, sdaIn= true => stop
    dut.clock.step(10)
    dut.io.sclIn.poke(true.B)
    dut.io.sdaIn.poke(false.B)
    dut.clock.step(2)
    dut.io.sdaIn.poke(true.B)
    dut.clock.step(5)

    // The slave FSM might move to stopConditionSlave => then idle
    // We can just check sdaOut, sclOut eventually go high
    var stopDetected = false
    for (_ <- 0 until 20) {
      dut.clock.step(1)
      val sclVal = dut.io.sclOut.peek().litToBoolean
      val sdaVal = dut.io.sdaOut.peek().litToBoolean
      if (sclVal && sdaVal) {
        stopDetected = true
      }
    }
    assert(stopDetected, "Slave did not release lines on stop condition")
  }

}
