package tech.rocksavage.chiselware.I2C

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.apb.ApbBundle
import tech.rocksavage.chiselware.apb.ApbTestUtils._

object receiveTests {

  /** MASTER DATA RECEIVE
    *  - The hardware latches bits on falling edges of sclOut
    *  - So the test must place each bit while sclOut=HIGH
    *  - Then step enough cycles so sclOut transitions from HIGH->LOW
    */
  def masterDataReceive(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // Setup: R/W=1 => 0xA1
    val maddrReg  = dut.registerMap.getAddressOfRegister("maddr").get
    val mbaudReg  = dut.registerMap.getAddressOfRegister("mbaud").get
    val mctrlaReg = dut.registerMap.getAddressOfRegister("mctrla").get
    writeAPB(dut.io.apb, maddrReg.U, 0xA1.U)
    writeAPB(dut.io.apb, mbaudReg.U, 2.U)
    writeAPB(dut.io.apb, mctrlaReg.U, 1.U)  // start

    // Wait ~40 => address phase
    dut.clock.step(40)

    // We want to feed 8 bits => 0xA5 = 165
    // The code latches them on falling edges => we place bits while sclOut=HIGH
    val dataByte = 0xA5
    for (i <- 0 until 8) {
      // Wait for sclOut => HIGH
      while(!dut.io.sclOut.peek().litToBoolean) {
        dut.clock.step(1)
      }
      // Now place the bit
      val bit = (dataByte >> (7 - i)) & 1
      dut.io.sdaIn.poke(bit.B)

      // Step ~2 cycles => let sclOut go from high->low => falling edge => latch
      dut.clock.step(2)
    }

    // Wait 20
    dut.clock.step(20)

    val mdataReg  = dut.registerMap.getAddressOfRegister("mdata").get
    val gotData   = readAPB(dut.io.apb, mdataReg.U).toInt
    assert(gotData == dataByte, s"Master got $gotData, expected $dataByte")

    val mstatusReg = dut.registerMap.getAddressOfRegister("mstatus").get
    val gotStatus  = readAPB(dut.io.apb, mstatusReg.U).toInt
    assert(gotStatus == 2, s"Master mstatus=$gotStatus, expected=2")
  }

  /** SLAVE ADDRESS RECEIVE
    *  - The slave code latches bits on falling edges => `when(!io.sclIn)`
    *  - So the test sets each bit while sclIn=HIGH, then drives sclIn=LOW => falling => latch
    */
  def slaveAddressReceive(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    val sctrlaReg = dut.registerMap.getAddressOfRegister("sctrla").get
    val saddrReg  = dut.registerMap.getAddressOfRegister("saddr").get
    writeAPB(dut.io.apb, sctrlaReg.U, 1.U)
    writeAPB(dut.io.apb, saddrReg.U, 0x50.U)

    // fake start => sclIn=1, sdaIn from 1->0
    dut.io.sclIn.poke(true.B)
    dut.io.sdaIn.poke(true.B)
    dut.clock.step(2)
    dut.io.sdaIn.poke(false.B)
    dut.clock.step(2)

    // feed 8 bits => 0xA0 => place bit while sclIn=HIGH, then sclIn=LOW => latch
    val addrByte = 0xA0
    for (i <- 0 until 8) {
      // sclIn=HIGH => set bit
      dut.io.sclIn.poke(true.B)
      val bit = (addrByte >> (7 - i)) & 1
      dut.io.sdaIn.poke(bit.B)
      dut.clock.step(1)

      // now sclIn= false => falling => latch
      dut.io.sclIn.poke(false.B)
      dut.clock.step(1)
    }

    dut.clock.step(5)

    val sstatusReg = dut.registerMap.getAddressOfRegister("sstatus").get
    val gotStatus  = readAPB(dut.io.apb, sstatusReg.U).toInt
    assert(gotStatus == 1, s"Slave address gotStatus=$gotStatus, expected=1")
  }

  /** SLAVE DATA RECEIVE
    *  - The slave code latches bits on falling edges => `when(!io.sclIn)`
    *  - So place each bit while sclIn=HIGH, then drive sclIn=LOW => latch
    */
  def slaveDataReceive(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    val sctrlaReg = dut.registerMap.getAddressOfRegister("sctrla").get
    writeAPB(dut.io.apb, sctrlaReg.U, 1.U)

    // fake start
    dut.io.sclIn.poke(true.B)
    dut.io.sdaIn.poke(true.B)
    dut.clock.step(2)
    dut.io.sdaIn.poke(false.B)
    dut.clock.step(2)

    val dataByte = 0xA5
    for (i <- 0 until 8) {
      // sclIn=HIGH => place bit
      dut.io.sclIn.poke(true.B)
      val bit = (dataByte >> (7 - i)) & 1
      dut.io.sdaIn.poke(bit.B)
      dut.clock.step(1)

      // now sclIn=LOW => falling => latch
      dut.io.sclIn.poke(false.B)
      dut.clock.step(1)
    }

    dut.clock.step(5)

    val sdataReg   = dut.registerMap.getAddressOfRegister("sdata").get
    val gotData    = readAPB(dut.io.apb, sdataReg.U).toInt
    assert(gotData == dataByte, s"Slave data=$gotData, expected=$dataByte")

    val sstatusReg = dut.registerMap.getAddressOfRegister("sstatus").get
    val gotStatus  = readAPB(dut.io.apb, sstatusReg.U).toInt
    assert(gotStatus == 2, s"Slave sstatus=$gotStatus, expected=2")
  }
}
