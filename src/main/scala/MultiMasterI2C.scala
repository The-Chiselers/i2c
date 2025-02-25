
package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap

class MultiMasterI2C(p: BaseParams, formal: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val masterApb1 = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val masterApb2 = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val slaveApb  = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    
    val master1 = new MasterInterface
    val master2 = new MasterInterface
    val slave  = new SlaveInterface
  })

  val master1 = Module(new I2C(p, formal))
  val master2 = Module(new I2C(p, formal))
  val slave  = Module(new I2C(p, formal))

  master1.io.apb <> io.masterApb1
  master2.io.apb <> io.masterApb2
  slave.io.apb <> io.slaveApb

  master1.io.master <> io.master1  
  master2.io.master <> io.master2
  slave.io.slave <> io.slave   


  //val sclWire = Wire(Bool())
  //sclWire := master1.io.master.sclOut & master2.io.master.sclOut
  slave.io.slave.sclIn := master1.io.master.sclOut & master2.io.master.sclOut & slave.io.slave.sclOut
  master1.io.master.sclIn := master1.io.master.sclOut & master2.io.master.sclOut & slave.io.slave.sclOut
  master2.io.master.sclIn := master1.io.master.sclOut & master2.io.master.sclOut & slave.io.slave.sclOut

  // Model open-drain behavior for SDA
  //val sdaWire = Wire(Bool())
  //sdaWire := master1.io.master.sdaOut & master2.io.master.sdaOut & slave.io.slave.sdaOut
  master1.io.master.sdaIn := master1.io.master.sdaOut & master2.io.master.sdaOut & slave.io.slave.sdaOut
  master2.io.master.sdaIn := master1.io.master.sdaOut & master2.io.master.sdaOut & slave.io.slave.sdaOut
  slave.io.slave.sdaIn := master1.io.master.sdaOut & master2.io.master.sdaOut & slave.io.slave.sdaOut
  

  master1.io.slave.sclIn := 0.U
  slave.io.master.sdaIn := 0.U
  master1.io.slave.sdaIn := 0.U
  master2.io.slave.sclIn := 0.U
  master2.io.slave.sdaIn := 0.U
  slave.io.master.sclIn := 0.U

  def getMaster1: I2C = master1
  def getMaster2: I2C = master2
  def getSlave: I2C = slave
  
  def getMasterRegisterMap1 = master1.registerMap
  def getMasterRegisterMap2 = master2.registerMap
  def getSlaveRegisterMap = slave.registerMap

  if (formal) {
    // no two masters should be transmit state at the same time
  
  }

}
  