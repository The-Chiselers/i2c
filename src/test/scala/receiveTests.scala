package tech.rocksavage.chiselware.I2C

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.apb.ApbBundle
import tech.rocksavage.chiselware.apb.ApbTestUtils._

object receiveTests {

  def masterDataReceive(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // 1) Setup R/W=1 => 0xA1
    val maddrReg  = dut.registerMap.getAddressOfRegister("maddr").get
    val mbaudReg  = dut.registerMap.getAddressOfRegister("mbaud").get
    val mctrlaReg = dut.registerMap.getAddressOfRegister("mctrla").get
    writeAPB(dut.io.apb, maddrReg.U, 0xA1.U)
    writeAPB(dut.io.apb, mbaudReg.U, 2.U)
    writeAPB(dut.io.apb, mctrlaReg.U, 1.U)  // start

    // Wait ~40 cycles for address phase
    dut.clock.step(40)

    // 2) We want to feed 8 bits => 0xA5 (165)
    // Because hardware samples on RISING edges, we place bits while sclOut=LOW.
    val dataByte = 0xA5
    for (i <- 0 until 8) {
      // Wait until sclOut=LOW
      while(dut.io.master.scl.peek().litToBoolean) {
        dut.clock.step(1)
      }
      // Now set the bit
      val bit = (dataByte >> (7 - i)) & 1
      dut.io.slave.sdaIn.poke(bit.B)

      // Step 2 cycles => let sclOut go LOW->HIGH => master latches on rising
      dut.clock.step(2)
    }

    // Wait 20
    dut.clock.step(20)

    // 3) Check mdata => should be 165
    val mdataReg = dut.registerMap.getAddressOfRegister("mdata").get
    val gotData  = readAPB(dut.io.apb, mdataReg.U).toInt
    assert(gotData == dataByte, s"Master got $gotData, expected $dataByte")

    val mstatusReg = dut.registerMap.getAddressOfRegister("mstatus").get
    val gotStatus  = readAPB(dut.io.apb, mstatusReg.U).toInt
    assert(gotStatus == 2, s"Master mstatus=$gotStatus, expected=2")
  }


  /** SLAVE ADDRESS => code uses rising edges in addressReceiveSlave
    * => place bit while sclIn=LOW, then sclIn=HIGH => rising => latch
    */
  def slaveAddressReceive(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock= dut.clock

    val sctrlaReg= dut.registerMap.getAddressOfRegister("sctrla").get
    val saddrReg = dut.registerMap.getAddressOfRegister("saddr").get
    writeAPB(dut.io.apb, sctrlaReg.U, 1.U)
    writeAPB(dut.io.apb, saddrReg.U, 0x50.U)

    // fake start => sclIn=1, sdaIn=1->0
    dut.io.slave.scl.poke(true.B)
    dut.io.slave.sdaIn.poke(true.B)
    dut.clock.step(2)
    dut.io.slave.sdaIn.poke(false.B)
    dut.clock.step(2)

    val addrByte= 0xA0
    for(i<-0 until 8){
      // place bit while sclIn=LOW
      dut.io.slave.scl.poke(false.B)
      val bit= (addrByte >> (7-i)) & 1
      dut.io.slave.sdaIn.poke(bit.B)
      dut.clock.step(1)

      // now sclIn= true => rising => latch
      dut.io.slave.scl.poke(true.B)
      dut.clock.step(1)
    }

    dut.clock.step(5)

    val sstatusReg= dut.registerMap.getAddressOfRegister("sstatus").get
    val gotStatus= readAPB(dut.io.apb, sstatusReg.U).toInt
    assert(gotStatus==1, s"Slave address gotStatus=$gotStatus, expected=1")
  }

  /** SLAVE DATA => code uses falling edges in dataReceiveSlave
    * => place bit while sclIn=HIGH, then sclIn=LOW => falling => latch
    */
  def slaveDataReceive(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock= dut.clock

    val sctrlaReg= dut.registerMap.getAddressOfRegister("sctrla").get
    writeAPB(dut.io.apb, sctrlaReg.U, 1.U)

    // fake start
    dut.io.slave.scl.poke(true.B)
    dut.io.slave.sdaIn.poke(true.B)
    dut.clock.step(2)
    dut.io.slave.sdaIn.poke(false.B)
    dut.clock.step(2)

    val dataByte= 0xA5
    for(i<-0 until 8){
      // sclIn=HIGH => place bit
      dut.io.slave.scl.poke(true.B)
      val bit= (dataByte >> (7-i)) &1
      dut.io.slave.sdaIn.poke(bit.B)
      dut.clock.step(1)

      // sclIn=LOW => falling => latch
      dut.io.slave.scl.poke(false.B)
      dut.clock.step(1)
    }

    dut.clock.step(5)

    val gotData= readAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("sdata").get.U).toInt
    assert(gotData== dataByte, s"Slave data=$gotData, expected=$dataByte")

    val sstatusReg= dut.registerMap.getAddressOfRegister("sstatus").get
    val gotStatus= readAPB(dut.io.apb, sstatusReg.U).toInt
    assert(gotStatus==2, s"Slave sstatus=$gotStatus, expected=2")
  }
}
