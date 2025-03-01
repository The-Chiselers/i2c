
package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap
import tech.rocksavage.chiselware.I2C.I2CEnums._
import chiseltest.formal.past

class MultiMasterI2C(p: BaseParams, formal: Boolean = false) extends Module {
  var io = IO(new Bundle {
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
  val master1State = master1.stateReg
  val master2State = master2.stateReg
  val slaveState   = slave.stateReg
  val pastMaster1State = RegNext(master1State, init = STATE_IDLE)
  val pastMaster2State = RegNext(master2State, init = STATE_IDLE)
  val pastSclBus   = RegNext(master1.io.master.sclOut & master2.io.master.sclOut & slave.io.slave.sclOut, init = true.B)
  val pastSdaBus   = RegNext(master1.io.master.sdaOut & master2.io.master.sdaOut & slave.io.slave.sdaOut, init = true.B)

  // 1) Mutual Exclusion Check (more flexible)
  when(master1.busStateReg === BusState.OWNER && master2.busStateReg === BusState.OWNER) {
    assert(false.B, "Both masters cannot be bus owners simultaneously")
  }

  // 2) Arbitration Resolution - Check transitions after arbitration
  when(past(master1State) === STATE_MASTERADDRESS && past(master2State) === STATE_MASTERADDRESS) {
    when(past(master1.io.master.sdaOut) =/= past(master1.io.master.sdaIn) && past(master1.io.master.sclOut) === 1.U) {
      assert(master1.busStateReg === BusState.BUSY, "Master1 should lose arbitration")
    }
    
    when(past(master2.io.master.sdaOut) =/= past(master2.io.master.sdaIn) && past(master2.io.master.sclOut) === 1.U) {
      assert(master2.busStateReg === BusState.BUSY, "Master2 should lose arbitration")
    }
  }

  // 3) Wired-AND Bus Logic
  assert(master1.io.master.sclIn === (master1.io.master.sclOut & master2.io.master.sclOut & slave.io.slave.sclOut), 
         "SCL line should follow wired-AND logic")
  assert(master1.io.master.sdaIn === (master1.io.master.sdaOut & master2.io.master.sdaOut & slave.io.slave.sdaOut), 
         "SDA line should follow wired-AND logic")

  // 4) Bus Ownership and ACK Relationship
  when(slaveState === STATE_SENDACKSLAVE && slave.io.slave.sdaOut === 0.U) {
    assert((master1.busStateReg === BusState.OWNER && master1State === STATE_WAITACKMASTER) || 
           (master2.busStateReg === BusState.OWNER && master2State === STATE_WAITACKMASTER),
           "Slave should ACK only when a master is waiting for ACK and owns the bus")
  }
  
  // 5) Bus Release After STOP
  when(past(master1State) === STATE_SENDSTOP && master1State === STATE_IDLE) {
    assert(master1.busStateReg === BusState.IDLE, "Master1 should release bus after STOP")
  }
  
  when(past(master2State) === STATE_SENDSTOP && master2State === STATE_IDLE) {
    assert(master2.busStateReg === BusState.IDLE, "Master2 should release bus after STOP")
  }
  
  // 6) Liveness - Ensure Progress
  val master1StableCounter = RegInit(0.U(8.W))
  val master2StableCounter = RegInit(0.U(8.W))
  
  when(master1State === pastMaster1State && master1State =/= STATE_IDLE) {
    master1StableCounter := master1StableCounter + 1.U
  } .otherwise {
    master1StableCounter := 0.U
  }
  
  when(master2State === pastMaster2State && master2State =/= STATE_IDLE) {
    master2StableCounter := master2StableCounter + 1.U
  } .otherwise {
    master2StableCounter := 0.U
  }
  
  assert(master1StableCounter < 150.U, "Master1 should progress or return to IDLE within a bounded time")
  assert(master2StableCounter < 150.U, "Master2 should progress or return to IDLE within a bounded time")
}
}
    