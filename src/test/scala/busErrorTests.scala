package tech.rocksavage.chiselware.I2C

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.apb.ApbBundle
import tech.rocksavage.chiselware.apb.ApbTestUtils._

/** Tests for bus error conditions:
  * e.g. if SDA changes unexpectedly while SCL=high
  */
object busErrorTests {

  /** Minimal example:
    *  1) Let the master or slave do something
    *  2) Force an illegal transition on SDA while SCL=1
    *  3) Check that the design sets a BUSERR bit
    */
  def masterBusErrorTest(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // Start master
    val mctrlAddr = dut.registerMap.getAddressOfRegister("mctrl").get
    writeAPB(dut.io.apb, mctrlAddr.U, 1.U)

    dut.clock.step(10)
    
    // Force an illegal move: SCL is high => forcibly toggle sdaIn
    // In a real design, we might wait exactly for sclOut=1
    while(!dut.io.master.sclOut.peek().litToBoolean) { dut.clock.step(1) }
    
    // Now quickly toggle sdaIn
    dut.io.slave.sdaIn.poke(false.B)
    dut.clock.step(2)
    dut.io.slave.sdaIn.poke(true.B)

    dut.clock.step(10)

    // Check busErr bit in mstatus => assume bit7
    val mstatusAddr = dut.registerMap.getAddressOfRegister("mstatus").get
    val finalStatus = readAPB(dut.io.apb, mstatusAddr.U)
    assert((finalStatus & 0x80) == 0x80, s"Expected bus error bit set. Got 0x${finalStatus.toString()}")
  }

  /** Similarly, we do a slave bus error test when our slave logic sets a bus error 
    * in an illegal scenario. The approach is the same: do a forced illegal transition 
    * and read a status bit in sstatus.
    */
  def slaveBusErrorTest(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // Enable slave
    val sctrlaAddr = dut.registerMap.getAddressOfRegister("sctrla").get
    writeAPB(dut.io.apb, sctrlaAddr.U, 1.U)

    dut.clock.step(5)

    // Now cause an illegal stop or sda toggling while sclIn=1
    // e.g. quickly toggle sdaIn while sclIn=1
    dut.io.slave.scl.poke(true.B)
    dut.io.slave.sdaIn.poke(false.B)
    dut.clock.step(2)
    dut.io.slave.sdaIn.poke(true.B)
    dut.clock.step(5)

    // Check sstatus for BUSERR bit => assume bit7 as well
    val sstatusAddr = dut.registerMap.getAddressOfRegister("sstatus").get
    val finalStat   = readAPB(dut.io.apb, sstatusAddr.U)
    assert((finalStat & 0x80) == 0x80, s"Expected slave bus error bit set. sstatus=0x${finalStat.toString()}")
  }

}
