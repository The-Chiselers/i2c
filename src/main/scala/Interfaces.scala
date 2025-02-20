package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import java.io.{File, PrintWriter}
import _root_.circt.stage.ChiselStage

class ApbInterface(p: BaseParams) extends Bundle {
  //val PCLK = Input(Clock())
  //val PRESETn = Input(AsynchReset())
  val PSEL = Input(Bool()) // Peripheral select
  val PENABLE = Input(Bool()) // Enable signal
  val PWRITE = Input(Bool()) // Read/Write signal
  val PADDR = Input(UInt(p.addrWidth.W)) // Address
  val PWDATA = Input(UInt(p.dataWidth.W)) // Write data
  val PRDATA = Output(UInt(p.dataWidth.W)) // Read data
  val PREADY = Output(Bool()) // Ready signal
  val PSLVERR = Output(Bool()) // Slave error signal
}

class MasterInterface() extends Bundle {
  val sclIn     = Input(Bool())
  val sclOut    = Output(Bool())  // I2C clock output (master)
  val sdaIn     = Input(UInt(1.W))
  val sdaOut    = Output(UInt(1.W))  // I2C data output (master)
}

class SlaveInterface() extends Bundle {
  val scl     = Input(Bool())   // I2C clock input (slave mode)
  val sdaIn     = Input(UInt(1.W))   // I2C data input (slave mode)'
  val sdaOut    = Output(UInt(1.W))
}

class I2CInterface(p: BaseParams) extends Bundle {
  val apb = new ApbInterface(p)
  val master = new MasterInterface
  val slave = new SlaveInterface
}