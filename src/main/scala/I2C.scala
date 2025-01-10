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
    val apb       = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val sclIn     = Input(Bool())  // I2C clock input (slave mode)
    val sdaIn     = Input(Bool())  // I2C data input  (slave mode)
    val sclOut    = Output(Bool()) // I2C clock output
    val sdaOut    = Output(Bool()) // I2C data output
    val interrupt = Output(Bool()) // Interrupt signal
  })

  // ======================================================================
  // Register Map
  // ======================================================================
  val registerMap = new RegisterMap(p.regWidth, p.addrWidth)

  // -----------------------------
  // Master Registers
  // -----------------------------
  val mctrla  = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mctrla, "mctrla")

  val mctrlb  = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mctrlb, "mctrlb")

  val mstatus = RegInit(0.U(p.regWidth.W))  // Master Status: bits for ARBLOST, BUSERR, etc.
  registerMap.createAddressableRegister(mstatus, "mstatus")

  val mbaud   = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mbaud, "mbaud")

  val maddr   = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(maddr, "maddr")

  val mdata   = RegInit(0.U(p.dataWidth.W))
  registerMap.createAddressableRegister(mdata, "mdata")

  // -----------------------------
  // Slave Registers
  // -----------------------------
  val sctrla    = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sctrla, "sctrla")

  val sctrlb    = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sctrlb, "scrtlb")

  val sstatus   = RegInit(0.U(p.regWidth.W))  // Slave Status: bits for COLL, BUSERR, etc.
  registerMap.createAddressableRegister(sstatus, "sstatus")

  val saddr     = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(saddr, "saddr")

  val sdata     = RegInit(0.U(p.dataWidth.W))
  registerMap.createAddressableRegister(sdata, "sdata")

  val saddrmask = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(saddrmask, "saddrmask")

  // ======================================================================
  // AddrDecode Integration
  // ======================================================================
  val addrDecodeParams = registerMap.getAddrDecodeParams
  val addrDecode       = Module(new AddrDecode(addrDecodeParams))
  addrDecode.io.addr       := io.apb.PADDR
  addrDecode.io.addrOffset := 0.U
  addrDecode.io.en         := true.B
  addrDecode.io.selInput   := true.B

  // ======================================================================
  // APB Interface
  // ======================================================================
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

  // ======================================================================
  // Clock Divider (Master)
  // ======================================================================
  val sclCnt       = RegInit(0.U(16.W))
  val sclToggleReg = RegInit(true.B)

  when(sclCnt === 0.U) {
    sclCnt       := mbaud
    sclToggleReg := ~sclToggleReg
  } .otherwise {
    sclCnt := sclCnt - 1.U
  }

  // ======================================================================
  // Bus Error Detection (Minimal Example)
  // ======================================================================
  // If SDA changes while SCL is high but not recognized as a Start or Stop, set bus error
  val sdaOld = RegNext(io.sdaOut & io.sdaIn) // "wired" combined
  val sclOld = RegNext(io.sclOut & io.sclIn)

  val busErr = RegInit(false.B)
  when(sclOld === true.B && (io.sclOut & io.sclIn) === true.B) {
    // If we see an unexpected transition on SDA here, busErr = true
    // This is a big simplification. Real logic must be more precise.
    when((io.sdaOut & io.sdaIn) =/= sdaOld) {
      busErr := true.B
    }
  }

  // Master might copy busErr => mstatus
  when(busErr) {
    mstatus := mstatus | "b10000000".U // e.g. bit7 => BUSERR
  }

  // ======================================================================
  //  Master FSM with Arbitration + Repeated Starts
  // ======================================================================
  val idleMaster             = 0.U
  val startConditionMaster   = 1.U
  val repeatedStartMaster    = 2.U
  val sendAddressMaster      = 3.U
  val waitAckMasterAddr      = 4.U
  val transmitDataMaster     = 5.U
  val waitAckMasterData      = 6.U
  val receiveDataMaster      = 7.U
  val waitAckMasterRx        = 8.U
  val arbitrationLostMaster  = 9.U
  val stopConditionMaster    = 10.U

  val masterFSM = RegInit(idleMaster)

  val sclMasterReg       = RegInit(true.B)
  val sdaMasterReg       = RegInit(true.B)

  // "Collision" detection => if we want SDA high, but the bus is forced low => ARB lost
  def masterDrivingLow() = !sdaMasterReg
  def busSDA()           = (io.sdaOut & io.sdaIn) // the actual bus line
  val arbLost            = RegInit(false.B)

  // Helper: check if master tries to drive SDA=1, but bus is 0
  def checkArbitration(): Unit = {
    when(sdaMasterReg === true.B && busSDA() === false.B) {
      // We tried to let SDA go high, but it's forced low => lost arbitration
      arbLost := true.B
      mstatus := mstatus | "b01000000".U // e.g. bit6 => ARBLOST
    }
  }

  // For receiving bits
  val bitCounter       = RegInit(0.U(4.W))
  val addressWithRW    = RegInit(0.U(8.W))
  val transmittedData  = RegInit(0.U(p.dataWidth.W))
  val receivedDataReg  = RegInit(0.U(p.dataWidth.W))

  switch(masterFSM) {
    is(idleMaster) {
      arbLost := false.B
      busErr  := false.B
      sclMasterReg := true.B
      sdaMasterReg := true.B
      bitCounter   := 0.U
      when(mctrla(0)) {
        masterFSM := startConditionMaster
      }
    }

    // Standard Start Condition
    is(startConditionMaster) {
      // If SCL & SDA high, drive SDA low => start
      when(sclMasterReg && sdaMasterReg) {
        sdaMasterReg := false.B
      }
      // Then drive SCL low
      .elsewhen(!sdaMasterReg && sclMasterReg) {
        sclMasterReg := false.B
        masterFSM    := sendAddressMaster
        bitCounter   := 0.U
      }
      checkArbitration()
      when(arbLost) {
        masterFSM := arbitrationLostMaster
      }
    }

    // Repeated Start => same as Start but occurs without a Stop
    is(repeatedStartMaster) {
      // Optionally we skip letting SDA go high if we are already driving it low
      when(sclMasterReg && sdaMasterReg) {
        sdaMasterReg := false.B
      } .elsewhen(!sdaMasterReg && sclMasterReg) {
        sclMasterReg := false.B
        masterFSM := sendAddressMaster
        bitCounter := 0.U
      }
      checkArbitration()
      when(arbLost) {
        masterFSM := arbitrationLostMaster
      }
    }

    is(sendAddressMaster) {
      val slaveAddr7 = maddr(7,1)
      val rwBit      = maddr(0)
      addressWithRW := Cat(slaveAddr7, rwBit)
      when(!sclMasterReg && sclToggleReg) {
        sdaMasterReg := addressWithRW(7.U - bitCounter)
        bitCounter := bitCounter + 1.U
        checkArbitration()
      }
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      when(arbLost) {
        masterFSM := arbitrationLostMaster
      }.elsewhen(bitCounter === 8.U) {
        masterFSM := waitAckMasterAddr
        bitCounter := 0.U
      }
    }

    is(waitAckMasterAddr) {
      // Release SDA for ACK
      when(!sclMasterReg && sclToggleReg) {
        sdaMasterReg := true.B
      }
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      when(sclMasterReg && sclToggleReg) {
        // read ack
        val ack = !busSDA()
        // If R/W=0 => transmit
        // If R/W=1 => receive
        val rwBit = addressWithRW(0)
        masterFSM := Mux(rwBit === 1.U, receiveDataMaster, transmitDataMaster)
      }
      checkArbitration()
      when(arbLost) {
        masterFSM := arbitrationLostMaster
      }
    }

    is(transmitDataMaster) {
      transmittedData := mdata
      when(!sclMasterReg && sclToggleReg) {
        sdaMasterReg := transmittedData(7.U - bitCounter)
        bitCounter := bitCounter + 1.U
        checkArbitration()
      }
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      when(arbLost) {
        masterFSM := arbitrationLostMaster
      }.elsewhen(bitCounter === 8.U) {
        masterFSM := waitAckMasterData
        bitCounter := 0.U
      }
    }

    is(waitAckMasterData) {
      when(!sclMasterReg && sclToggleReg) {
        sdaMasterReg := true.B
      }
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      when(sclMasterReg && sclToggleReg) {
        val ack = !busSDA()
        // For demonstration, we either stop or do repeated start if mctrlb says so
        // Suppose mctrlb(0)=1 => repeated start
        when(mctrlb(0)) {
          masterFSM := repeatedStartMaster
        } .otherwise {
          masterFSM := stopConditionMaster
        }
      }
      checkArbitration()
      when(arbLost) {
        masterFSM := arbitrationLostMaster
      }
    }

    is(receiveDataMaster) {
      // On rising edges, read busSDA
      when(sclMasterReg && sclToggleReg) {
        receivedDataReg := Cat(receivedDataReg(p.dataWidth-2,0), busSDA())
        bitCounter := bitCounter + 1.U
      }
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      when(arbLost) {
        masterFSM := arbitrationLostMaster
      }.elsewhen(bitCounter === 8.U) {
        mdata := receivedDataReg
        mstatus := 2.U // e.g. data recv
        masterFSM := waitAckMasterRx
        bitCounter := 0.U
      }
    }

    is(waitAckMasterRx) {
      // Suppose we always ACK for simplicity
      when(!sclMasterReg && sclToggleReg) {
        sdaMasterReg := false.B // ack=0
        checkArbitration()
      }
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      when(sclMasterReg && sclToggleReg) {
        // ack done => check repeated start or stop
        when(mctrlb(0)) {
          masterFSM := repeatedStartMaster
        } .otherwise {
          masterFSM := stopConditionMaster
        }
      }
    }

    is(arbitrationLostMaster) {
      // Once ARB is lost, release lines, set status bits
      sdaMasterReg := true.B
      sclMasterReg := true.B
      // stay here until bus is idle or user resets?
      when(!mctrla(0)) {
        masterFSM := idleMaster
      }
    }

    is(stopConditionMaster) {
      when(!sclMasterReg && sclToggleReg) {
        sclMasterReg := true.B
      } .elsewhen(sclMasterReg && !sdaMasterReg && sclToggleReg) {
        sdaMasterReg := true.B
      } .elsewhen(sclMasterReg && sdaMasterReg) {
        masterFSM := idleMaster
      }
    }
  }

  // ======================================================================
  // Slave FSM with Collision + BusError detection
  // ======================================================================
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

  // Minimal collision detection: if slave tries to drive SDA=0 but bus is forced 1 => collision
  def slaveCheckCollision(): Unit = {
    // "Bus" is (io.sdaOut & io.sdaIn) 
    val busVal = io.sdaIn & io.sdaOut
    when(!sdaSlaveReg && busVal === true.B) {
      sstatus := sstatus | "b00100000".U // e.g. bit5 => COLL
      slaveFSM := collisionSlave
    }
  }

  switch(slaveFSM) {
    is(idleSlave) {
      sstatus := 0.U
      when(slaveEnabled && !io.sdaIn && io.sclIn) {
        slaveFSM := addressReceiveSlave
        slaveBitCounter := 0.U
        slaveShiftReg   := 0.U
      }
    }

    is(addressReceiveSlave) {
      when(io.sclIn) {
        slaveShiftReg := Cat(slaveShiftReg(p.dataWidth-2,0), (io.sdaIn & io.sdaOut))
        slaveBitCounter := slaveBitCounter + 1.U
      }
      when(slaveBitCounter === 7.U && io.sclIn) {
        slaveFSM := addressAckSlave
      }
    }

    is(addressAckSlave) {
      val receivedAddr7 = slaveShiftReg(7,1)
      val rwBit         = slaveShiftReg(0)
      // if matched => sstatus=1
      when(receivedAddr7 === saddr(7,1)) {
        sstatus := 1.U
        sdaSlaveReg := false.B // ack
      } .otherwise {
        sstatus := 0.U
        sdaSlaveReg := true.B  // nack
      }
      slaveFSM := Mux(rwBit===1.U, dataTransmitSlave, dataReceiveSlave)
      slaveBitCounter := 0.U
    }

    is(dataReceiveSlave) {
      when(io.sclIn) {
        slaveShiftReg := Cat(slaveShiftReg(p.dataWidth-2,0), (io.sdaIn & io.sdaOut))
        slaveBitCounter := slaveBitCounter + 1.U
      }
      when(slaveBitCounter === 7.U && io.sclIn) {
        slaveFSM := dataAckSlave
      }
    }

    is(dataAckSlave) {
      sdata := slaveShiftReg
      sstatus := 2.U // data received
      sdaSlaveReg := false.B
      slaveBitCounter := 0.U
      slaveFSM := dataReceiveSlave
    }

    is(dataTransmitSlave) {
      // On falling edge, shift out bits
      when(!io.sclIn) {
        sdaSlaveReg := slaveShiftReg(p.dataWidth-1)
        slaveShiftReg := Cat(slaveShiftReg(p.dataWidth-2,0), 0.U)
        slaveBitCounter := slaveBitCounter + 1.U
        slaveCheckCollision()
      }
      when(slaveBitCounter===8.U) {
        slaveFSM := dataAckSlave
        slaveBitCounter := 0.U
      }
    }

    is(stopConditionSlave) {
      sdaSlaveReg := true.B
      sclSlaveReg := true.B
      slaveFSM := idleSlave
    }

    is(collisionSlave) {
      // if collision => release lines
      sdaSlaveReg := true.B
      sclSlaveReg := true.B
      // wait for bus idle again
      when(io.sdaIn && io.sclIn) {
        slaveFSM := idleSlave
      }
    }

    is(busErrSlave) {
      // release lines, wait for reset
      sdaSlaveReg := true.B
      sclSlaveReg := true.B
      when(!slaveEnabled) {
        slaveFSM := idleSlave
      }
    }
  }

  // Minimal detection of a "stop" => SCL high, SDA goes high => shift states
  // ======================================================================
  // Drive Outputs (wired-AND style)
  // ======================================================================
  io.sclOut := sclSlaveReg & sclMasterReg
  io.sdaOut := sdaSlaveReg & sdaMasterReg

  // Basic interrupt example
  io.interrupt := (mstatus & "b11000000".U) =/= 0.U || (sstatus & "b00100000".U) =/= 0.U
}
