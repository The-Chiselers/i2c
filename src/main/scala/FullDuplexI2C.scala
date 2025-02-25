package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import chiseltest.formal.past
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap

class FullDuplexI2C(p: BaseParams, formal: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val masterApb = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val slaveApb  = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    
    val master = new MasterInterface
    val slave  = new SlaveInterface
  })

  val master = Module(new I2C(p, formal))
  val slave  = Module(new I2C(p, formal))

  master.io.apb <> io.masterApb
  slave.io.apb <> io.slaveApb

  master.io.master <> io.master  
  slave.io.slave <> io.slave  

  master.io.master.sclIn := master.io.master.sclOut & slave.io.slave.sclOut
  slave.io.slave.sclIn := master.io.master.sclOut & slave.io.slave.sclOut

  // Model open-drain behavior for SDA
  //val sdaWire = Wire(Bool())
  //sdaWire := master.io.master.sdaOut & slave.io.slave.sdaOut
  master.io.master.sdaIn := master.io.master.sdaOut & slave.io.slave.sdaOut
  slave.io.slave.sdaIn := master.io.master.sdaOut & slave.io.slave.sdaOut
  /*
  slave.io.slave.sdaIn   := master.io.master.sdaOut    

  master.io.master.sdaIn := slave.io.slave.sdaOut  
  */

  master.io.slave.sclIn := 0.U
  slave.io.master.sdaIn := 0.U
  master.io.slave.sdaIn := 0.U
  slave.io.master.sclIn := 0.U


  def getMaster: I2C = master
  def getSlave: I2C = slave

  def getMasterRegisterMap = master.registerMap
  def getSlaveRegisterMap = slave.registerMap

  if (formal) {
    val masterWrite = master.io.state === 7.U //STATE_MASTERWRITE
    val masterRead = master.io.state === 9.U //STATE_MASTERREAD
    val mastSclHasGoneHigh = RegInit(false.B)
    when(masterWrite) {
      when(master.io.master.sclOut && !past(master.io.master.sclOut)) {
        mastSclHasGoneHigh := true.B
      }
      when(mastSclHasGoneHigh) {
        assert(master.io.master.sdaOut === 0.U)
      }
      // once a clock has gone high, sda must be low
      
      
    }
    when(masterRead) {
      assert(master.io.master.sclOut && master.io.master.sdaOut === 0.U)
    }
    // assert(master.io.state === STATE_MASTERWRITE && past(!master.io.master.sclOut) && master.io.master.sdaOut === 1.U)
    // assert(master.io.state === STATE_MASTERREAD && past(!master.io.master.sclOut) && master.io.master.sdaOut === 1.U)
  }

}

