/*

// (c) 2025 Example Only. Not production-ready.
// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)
package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap

class I2C(p: BaseParams) extends Module {
  val io = IO(new Bundle {
    val apb = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val master = new MasterInterface
    val slave = new SlaveInterface
    val interrupt = Output(Bool())  // Interrupt
  })

  // ------------------------------------------------------------------------
  // Register Map
  // ------------------------------------------------------------------------
  val registerMap = new RegisterMap(p.regWidth, p.addrWidth)

  // Master registers
  val mctrla  = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mctrla,  "mctrla")

  val mctrlb  = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mctrlb,  "mctrlb")

  val mstatus = RegInit(0.U(p.regWidth.W))  // bits for ARBLOST, BUSERR
  registerMap.createAddressableRegister(mstatus, "mstatus")

  val mbaud   = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mbaud,   "mbaud")

  val maddr   = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(maddr,   "maddr")

  val mdata   = RegInit(0.U(p.dataWidth.W))
  registerMap.createAddressableRegister(mdata,   "mdata")

  // Slave registers
  val sctrla    = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sctrla,    "sctrla")

  val sctrlb    = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sctrlb,    "scrtlb")

  val sstatus   = RegInit(0.U(p.regWidth.W)) // bits for COLL, BUSERR, etc.
  registerMap.createAddressableRegister(sstatus,   "sstatus")

  val saddr     = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(saddr,     "saddr")

  val sdata     = RegInit(0.U(p.dataWidth.W))
  registerMap.createAddressableRegister(sdata,     "sdata")

  val saddrmask = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(saddrmask, "saddrmask")

  // ------------------------------------------------------------------------
  // AddrDecode Integration
  // ------------------------------------------------------------------------
  val addrDecodeParams = registerMap.getAddrDecodeParams
  val addrDecode       = Module(new AddrDecode(addrDecodeParams))
  addrDecode.io.addr       := io.apb.PADDR
  addrDecode.io.addrOffset := 0.U
  addrDecode.io.en         := true.B
  addrDecode.io.selInput   := true.B

  // ------------------------------------------------------------------------
  // APB Interface
  // ------------------------------------------------------------------------
  io.apb.PREADY  := (io.apb.PENABLE && io.apb.PSEL)
  io.apb.PSLVERR := (addrDecode.io.errorCode === AddrDecodeError.AddressOutOfRange)
  io.apb.PRDATA  := 0.U

  when(io.apb.PSEL && io.apb.PENABLE) {
    when(io.apb.PWRITE) {
      for (reg <- registerMap.getRegisters) {
        when(addrDecode.io.sel(reg.id)) {
          reg.writeCallback(addrDecode.io.addrOffset, io.apb.PWDATA)
        }
      }
    } .otherwise {
      for (reg <- registerMap.getRegisters) {
        when(addrDecode.io.sel(reg.id)) {
          io.apb.PRDATA := reg.readCallback(addrDecode.io.addrOffset)
        }
      }
    }
  }

  // ------------------------------------------------------------------------
  // Baud Rate Generator (Master)
  // ------------------------------------------------------------------------
  val sclCounter    = RegInit(0.U(16.W))
  val sclReg        = RegInit(false.B)

  when(mctrla(0) === 1.U){
    val fscl = (p.clkFreq.U * 1000000.U) / (10.U + 2.U * mbaud)
    val sclPeriod = (p.clkFreq.U * 1000000.U) / (2.U * fscl)
    when(sclCounter === (sclPeriod - 1.U)) {
      sclCounter := 0.U
      sclReg := ~sclReg
    } .otherwise {
      sclCounter := sclCounter + 1.U
    }
    io.master.scl := sclReg
  }.otherwise {
    io.master.scl := 0.U
    io.master.sdaOut := 0.U
  }
  io.master.sdaOut := 0.U //Temp

  // ------------------------------------------------------------------------
  // Bus Error Detection (Minimal Example)
  // ------------------------------------------------------------------------
  val sdaOld = RegNext(io.master.sdaOut & io.master.sdaIn) 
  val sclOld = RegNext(io.master.scl & io.slave.scl)

  val busErr = RegInit(false.B)
  when(sclOld === true.B && (io.master.scl & io.slave.scl) === true.B) {
    when((io.master.sdaOut & io.master.sdaIn) =/= sdaOld) {
      busErr := true.B
    }
  }
  // if busErr => set bit7
  when(busErr) {
    mstatus := mstatus | "b10000000".U
  }

  // ------------------------------------------------------------------------
  // Master FSM with Arbitration, Repeated Start
  // ------------------------------------------------------------------------
  val idleMaster           = 0.U
  val startConditionMaster = 1.U
  val repeatedStartMaster  = 2.U
  val sendAddressMaster    = 3.U
  val waitAckMasterAddr    = 4.U
  val transmitDataMaster   = 5.U
  val waitAckMasterData    = 6.U
  val receiveDataMaster    = 7.U
  val waitAckMasterRx      = 8.U
  val arbitrationLostMaster= 9.U
  val stopConditionMaster  = 10.U

  val masterFSM = RegInit(idleMaster)

  val sclMasterReg = RegInit(true.B)
  val sdaMasterReg = RegInit(true.B)

  val arbLost     = RegInit(false.B)
  def busSDA()    = (io.master.sdaOut & io.master.sdaIn)

  // If we want SDA=1 but bus is 0 => lost arb
  def checkArbitration(): Unit = {
    when(sdaMasterReg && busSDA()===false.B) {
      arbLost := true.B
      mstatus := mstatus | "b01000000".U // bit6 => ARBLOST
    }
  }

  val bitCounter      = RegInit(0.U(4.W))
  val addressWithRW   = RegInit(0.U(8.W))
  val transmittedData = RegInit(0.U(p.dataWidth.W))
  val receivedDataReg = RegInit(0.U(p.dataWidth.W))

  switch(masterFSM) {
    is(idleMaster) {
      arbLost := false.B
      busErr  := false.B
      sclMasterReg := true.B
      sdaMasterReg := true.B
      bitCounter   := 0.U

      when(mctrla(0)) { // start
        masterFSM := startConditionMaster
      }
    }

    is(startConditionMaster) {
      // SDA low => start, then SCL low
      when(sclMasterReg && sdaMasterReg) {
        sdaMasterReg := false.B
      }.elsewhen(!sdaMasterReg && sclMasterReg) {
        sclMasterReg := false.B
        masterFSM    := sendAddressMaster
        bitCounter   := 0.U
      }
      checkArbitration()
      when(arbLost) { masterFSM := arbitrationLostMaster }
    }

    is(repeatedStartMaster) {
      // same as start but no stop
      when(sclMasterReg && sdaMasterReg) {
        sdaMasterReg := false.B
      }.elsewhen(!sdaMasterReg && sclMasterReg) {
        sclMasterReg := false.B
        masterFSM    := sendAddressMaster
        bitCounter   := 0.U
      }
      checkArbitration()
      when(arbLost) { masterFSM := arbitrationLostMaster }
    }

    is(sendAddressMaster) {
      val slave7 = maddr(7,1)
      val rwBit  = maddr(0)
      addressWithRW := Cat(slave7, rwBit)

      // SHIFT out on falling edges
      when(!sclMasterReg && sclReg) {
        sdaMasterReg := addressWithRW(7.U - bitCounter)
        bitCounter := bitCounter + 1.U
        checkArbitration()
      }
      when(sclReg) {
        sclMasterReg := !sclMasterReg
      }
      when(arbLost) {
        masterFSM := arbitrationLostMaster
      }.elsewhen(bitCounter===8.U) {
        bitCounter:=0.U
        masterFSM := waitAckMasterAddr
      }
    }

    is(waitAckMasterAddr) {
      // release for ack
      when(!sclMasterReg && sclReg) {
        sdaMasterReg := true.B
      }
      when(sclReg) {
        sclMasterReg := !sclMasterReg
      }
      // on rising => read ack => pick TX or RX
      when(sclMasterReg && sclReg) {
        val ack= !busSDA()
        val rw= addressWithRW(0)
        masterFSM := Mux(rw===1.U, receiveDataMaster, transmitDataMaster)
      }
      checkArbitration()
      when(arbLost){ masterFSM := arbitrationLostMaster}
    }

    is(transmitDataMaster) {
      transmittedData := mdata
      // SHIFT out on falling
      when(!sclMasterReg && sclReg) {
        sdaMasterReg := transmittedData(7.U - bitCounter)
        bitCounter := bitCounter+1.U
        checkArbitration()
      }
      when(sclReg) {
        sclMasterReg := !sclMasterReg
      }
      when(arbLost) {
        masterFSM := arbitrationLostMaster
      }.elsewhen(bitCounter===8.U) {
        bitCounter:=0.U
        masterFSM:= waitAckMasterData
      }
    }

    is(waitAckMasterData) {
      when(!sclMasterReg && sclReg) {
        sdaMasterReg := true.B
      }
      when(sclReg) {
        sclMasterReg := !sclMasterReg
      }
      when(sclMasterReg && sclReg){
        val ack= !busSDA()
        when(mctrlb(0)){ masterFSM:= repeatedStartMaster}
        .otherwise{masterFSM:= stopConditionMaster}
      }
      checkArbitration()
      when(arbLost){masterFSM:=arbitrationLostMaster}
    }

    is(receiveDataMaster) {
      // SHIFT in on RISING edges
      when(sclMasterReg && sclReg) {
        val bitIn= busSDA()
        receivedDataReg:= Cat(receivedDataReg(p.dataWidth-2,0), bitIn)
        bitCounter:= bitCounter+1.U
      }
      when(sclReg) {
        sclMasterReg:= !sclMasterReg
      }
      when(arbLost){
        masterFSM:= arbitrationLostMaster
      }.elsewhen(bitCounter===8.U){
        mdata   := receivedDataReg
        mstatus := 2.U // data recv
        bitCounter:=0.U
        masterFSM:= waitAckMasterRx
      }
    }

    is(waitAckMasterRx) {
      // always ack => sda=0 on falling
      when(!sclMasterReg && sclReg){
        sdaMasterReg:= false.B
        checkArbitration()
      }
      when(sclReg){ sclMasterReg:= !sclMasterReg}
      when(sclMasterReg && sclReg){
        when(mctrlb(0)){ masterFSM:= repeatedStartMaster}
        .otherwise{masterFSM:= stopConditionMaster}
      }
    }

    is(arbitrationLostMaster) {
      sdaMasterReg:= true.B
      sclMasterReg:= true.B
      when(!mctrla(0)){ masterFSM:= idleMaster}
    }

    is(stopConditionMaster){
      when(!sclMasterReg && sclReg){
        sclMasterReg:= true.B
      }.elsewhen(sclMasterReg && !sdaMasterReg && sclReg){
        sdaMasterReg:= true.B
      }.elsewhen(sclMasterReg && sdaMasterReg){
        masterFSM:= idleMaster
      }
    }
  }

  // ------------------------------------------------------------------------
  // Slave FSM with Collision + BusErr
  // ------------------------------------------------------------------------
  val idleSlave          = 0.U
  val addressReceiveSlave= 1.U
  val addressAckSlave    = 2.U
  val dataReceiveSlave   = 3.U
  val dataAckSlave       = 4.U
  val dataTransmitSlave  = 5.U
  val stopConditionSlave = 6.U
  val collisionSlave     = 7.U
  val busErrSlave        = 8.U

  val slaveFSM      = RegInit(idleSlave)
  val sdaSlaveReg   = RegInit(true.B)
  val sclSlaveReg   = RegInit(true.B)

  val slaveBitCounter = RegInit(0.U(4.W))
  val slaveShiftReg   = RegInit(0.U(p.dataWidth.W))
  val slaveEnabled    = sctrla(0)

  def slaveCheckCollision(): Unit = {
    val busVal= io.slave.sdaIn & io.slave.sdaOut
    when(!sdaSlaveReg && busVal===true.B){
      sstatus:= sstatus | "b00100000".U
      slaveFSM:= collisionSlave
    }
  }

  switch(slaveFSM) {
    is(idleSlave){
      sstatus:=0.U
      when(slaveEnabled && !io.slave.sdaIn && io.slave.scl){
        slaveFSM:= addressReceiveSlave
        slaveBitCounter:=0.U
        slaveShiftReg:=0.U
      }
    }
    is(addressReceiveSlave){
      // SHIFT in on rising edges of sclIn
      when(io.slave.scl){
        slaveShiftReg:= Cat(slaveShiftReg(p.dataWidth-2,0), (io.slave.sdaIn & io.slave.sdaOut))
        slaveBitCounter:= slaveBitCounter+1.U
      }
      when(slaveBitCounter===7.U && io.slave.scl){
        slaveFSM:= addressAckSlave
      }
    }
    is(addressAckSlave){
      val receivedAddr7= slaveShiftReg(7,1)
      val rwBit= slaveShiftReg(0)
      when(receivedAddr7=== saddr(7,1)){
        sstatus:=1.U
        sdaSlaveReg:= false.B // ack
      }.otherwise{
        sstatus:=0.U
        sdaSlaveReg:= true.B  //nack
      }
      slaveFSM:= Mux(rwBit===1.U, dataTransmitSlave, dataReceiveSlave)
      slaveBitCounter:=0.U
    }
    is(dataReceiveSlave){
      // SHIFT in on rising edges => your code says 'when(io.slave.scl)'
      when(io.slave.scl){
        slaveShiftReg:= Cat(slaveShiftReg(p.dataWidth-2,0), (io.slave.sdaIn & io.slave.sdaOut))
        slaveBitCounter:= slaveBitCounter+1.U
      }
      when(slaveBitCounter===7.U && io.slave.scl){
        slaveFSM:= dataAckSlave
      }
    }
    is(dataAckSlave){
      sdata:= slaveShiftReg
      sstatus:=2.U
      sdaSlaveReg:= false.B
      slaveBitCounter:=0.U
      slaveFSM:= dataReceiveSlave
    }
    is(dataTransmitSlave){
      // SHIFT out on falling edges
      when(!io.slave.scl){
        sdaSlaveReg:= slaveShiftReg(p.dataWidth-1)
        slaveShiftReg:= Cat(slaveShiftReg(p.dataWidth-2,0), 0.U)
        slaveBitCounter:= slaveBitCounter+1.U
        slaveCheckCollision()
      }
      when(slaveBitCounter===8.U){
        slaveFSM:= dataAckSlave
        slaveBitCounter:=0.U
      }
    }
    is(stopConditionSlave){
      sdaSlaveReg:= true.B
      sclSlaveReg:= true.B
      slaveFSM:= idleSlave
    }
    is(collisionSlave){
      sdaSlaveReg:=true.B
      sclSlaveReg:=true.B
      when(io.slave.sdaIn && io.slave.scl){
        slaveFSM:= idleSlave
      }
    }
    is(busErrSlave){
      sdaSlaveReg:=true.B
      sclSlaveReg:=true.B
      when(!slaveEnabled){
        slaveFSM:= idleSlave
      }
    }
  }

  // ------------------------------------------------------------------------
  // Combine (wired-AND)
  // ------------------------------------------------------------------------
  //io.master.scl := sclSlaveReg & sclMasterReg
  io.slave.sdaOut := sdaSlaveReg & sdaMasterReg

  // Basic interrupt => e.g. if bus error or collision
  io.interrupt := (mstatus & "b11000000".U) =/= 0.U || (sstatus & "b00100000".U) =/= 0.U
}

*/

