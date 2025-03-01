package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import chiseltest.formal.past
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap
import tech.rocksavage.chiselware.I2C.I2CEnums._

class FullDuplexI2C(p: BaseParams, formal: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val masterApb = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val slaveApb  = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val master = new MasterInterface
    val slave  = new SlaveInterface
    val interrupt = Output(Bool())
  })

  val master = Module(new I2C(p, formal))
  val slave  = Module(new I2C(p, formal))
  io.interrupt := master.io.interrupt | slave.io.interrupt

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
  val masterState = master.stateReg
  val slaveState  = slave.stateReg
  val pastMasterState = RegNext(masterState, init = STATE_IDLE)
  val pastSlaveState = RegNext(slaveState, init = STATE_IDLE)
  val pastSclBus  = RegNext(master.io.master.sclOut & slave.io.slave.sclOut, init = true.B)
  val pastSdaBus  = RegNext(master.io.master.sdaOut & slave.io.slave.sdaOut, init = true.B)

  // 1) Bidirectional Data Integrity
  when(masterState === STATE_MASTERWRITE && slaveState === STATE_SLAVEREAD && past(master.io.master.sclOut) === 0.U && master.io.master.sclOut === 1.U) {
    assert(slave.io.slave.sdaIn === master.io.master.sdaOut, "Slave should see master's data during write")
  }
  
  when(slaveState === STATE_SLAVEWRITE && masterState === STATE_MASTERREAD && past(master.io.master.sclOut) === 0.U && master.io.master.sclOut === 1.U) {
    assert(master.io.master.sdaIn === slave.io.slave.sdaOut, "Master should see slave's data during read")
  }

  // 2) Clock Synchronization
  when(master.io.master.sclOut === 0.U) {
    assert(slave.io.slave.sclIn === 0.U, "SCL should be low for slave when master drives low")
  }
  
  when(slave.io.slave.sclOut === 0.U) {
    assert(master.io.master.sclIn === 0.U, "SCL should be low for master when slave drives low")
  }
  
  // 3) Clock Stretching
  when(slaveState === STATE_SENDACKSLAVE && slave.sctrl(4) === 1.U) {
    assert(slave.io.slave.sclOut === 0.U, "Slave should stretch clock when sctrl(4) is set")
    assert(master.io.master.sclIn === 0.U, "Master should see stretched clock")
  }

  // 4) Start/Stop Condition Verification
  when(past(master.io.master.sclOut) === 1.U && master.io.master.sclOut === 1.U) {
    when(past(master.io.master.sdaOut) === 1.U && master.io.master.sdaOut === 0.U) {
      assert(pastMasterState === STATE_IDLE && masterState === STATE_MASTERADDRESS, 
             "START condition should transition from IDLE to MASTERADDRESS")
    }
    
    when(past(master.io.master.sdaOut) === 0.U && master.io.master.sdaOut === 1.U && pastMasterState === STATE_SENDSTOP) {
      assert(masterState === STATE_IDLE, "STOP condition should return to IDLE")
    }
  }

  // 5) Liveness - Ensure Progress
  val masterStableCounter = RegInit(0.U(8.W))
  val slaveStableCounter = RegInit(0.U(8.W))
  
  when(masterState === pastMasterState && masterState =/= STATE_IDLE) {
    masterStableCounter := masterStableCounter + 1.U
  } .otherwise {
    masterStableCounter := 0.U
  }
  
  when(slaveState === pastSlaveState && slaveState =/= STATE_IDLE && slaveState =/= STATE_WAITSTOP) {
    slaveStableCounter := slaveStableCounter + 1.U
  } .otherwise {
    slaveStableCounter := 0.U
  }
  
  assert(masterStableCounter < 150.U, "Master should progress or return to IDLE within a bounded time")
  assert(slaveStableCounter < 150.U, "Slave should progress or wait for STOP within a bounded time")
}

}

