package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap

class FullDuplexI2C(p: BaseParams) extends Module {
  val io = IO(new Bundle {
    // Expose the master and slave APB interfaces
    val masterApb = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val slaveApb  = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    
    // Expose the master and slave SPI interfaces for Full Duplex operation
    val master = new MasterInterface
    val slave  = new SlaveInterface
  })

  // Instantiate the SPI master and slave 
  val master = Module(new I2C(p))
  val slave  = Module(new I2C(p))

  // Connect the APB interface to both master and slave
  master.io.apb <> io.masterApb
  slave.io.apb <> io.slaveApb

  // Connect the SPI signals for Full Duplex
  // Master -> Slave
  master.io.master <> io.master  // Master drives SCLK, MOSI, and CS
  slave.io.slave <> io.slave    // Slave drives MISO
  slave.io.slave.scl := master.io.master.scl  // Slave gets the SCLK from master
  slave.io.slave.sdaIn   := master.io.master.sdaOut    // Slave gets CS from master
  
  // Slave -> Master
  master.io.master.sdaIn := slave.io.slave.sdaOut  // Master gets MISO from slave

  master.io.slave.scl := 0.U
  slave.io.master.sdaIn := 0.U
  master.io.slave.sdaIn := 0.U
  // Getter methods to access internal SPI modules
  def getMaster: I2C = master
  def getSlave: I2C = slave
  // Getter methods to access registerMap from the master and slave
  def getMasterRegisterMap = master.registerMap
  def getSlaveRegisterMap = slave.registerMap

}