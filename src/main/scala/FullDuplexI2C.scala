package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap

class FullDuplexI2C(p: BaseParams) extends Module {
  val io = IO(new Bundle {
    val masterApb = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val slaveApb  = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    
    val master = new MasterInterface
    val slave  = new SlaveInterface
  })

  val master = Module(new I2C(p))
  val slave  = Module(new I2C(p))

  master.io.apb <> io.masterApb
  slave.io.apb <> io.slaveApb

  master.io.master <> io.master  
  slave.io.slave <> io.slave  

  master.io.master.sclIn := true.B
  slave.io.slave.scl := master.io.master.sclOut  

  // Model open-drain behavior for SDA
  val sdaWire = Wire(Bool())
  sdaWire := master.io.master.sdaOut & slave.io.slave.sdaOut
  master.io.master.sdaIn := sdaWire
  slave.io.slave.sdaIn := sdaWire
  /*
  slave.io.slave.sdaIn   := master.io.master.sdaOut    

  master.io.master.sdaIn := slave.io.slave.sdaOut  
  */

  master.io.slave.scl := 0.U
  slave.io.master.sdaIn := 0.U
  master.io.slave.sdaIn := 0.U
  slave.io.master.sclIn := 0.U


  def getMaster: I2C = master
  def getSlave: I2C = slave

  def getMasterRegisterMap = master.registerMap
  def getSlaveRegisterMap = slave.registerMap

}

