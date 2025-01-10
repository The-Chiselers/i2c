// (c) 2025 Minimal I2C - Falling Edge Sample
package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._

// Minimal placeholders
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode._
import tech.rocksavage.chiselware.addressable.RegisterMap

class I2C(p: BaseParams) extends Module {
  val io = IO(new Bundle {
    val apb    = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val sclIn  = Input(Bool())
    val sdaIn  = Input(Bool())
    val sclOut = Output(Bool())
    val sdaOut = Output(Bool())

    val interrupt = Output(Bool())
  })

  // ------------------------------------------------------------------------
  // Register Map
  // ------------------------------------------------------------------------
  val registerMap = new RegisterMap(p.regWidth, p.addrWidth)
  val mctrla  = RegInit(0.U(p.regWidth.W));  registerMap.createAddressableRegister(mctrla,  "mctrla")
  val mstatus = RegInit(0.U(p.regWidth.W));  registerMap.createAddressableRegister(mstatus, "mstatus")
  val mbaud   = RegInit(0.U(p.regWidth.W));  registerMap.createAddressableRegister(mbaud,   "mbaud")
  val maddr   = RegInit(0.U(p.regWidth.W));  registerMap.createAddressableRegister(maddr,   "maddr")
  val mdata   = RegInit(0.U(p.dataWidth.W)); registerMap.createAddressableRegister(mdata,   "mdata")

  val sctrla  = RegInit(0.U(p.regWidth.W));  registerMap.createAddressableRegister(sctrla,  "sctrla")
  val sstatus = RegInit(0.U(p.regWidth.W));  registerMap.createAddressableRegister(sstatus, "sstatus")
  val saddr   = RegInit(0.U(p.regWidth.W));  registerMap.createAddressableRegister(saddr,   "saddr")
  val sdata   = RegInit(0.U(p.dataWidth.W)); registerMap.createAddressableRegister(sdata,   "sdata")

  // Minimal APB interface
  val addrDecodeParams = registerMap.getAddrDecodeParams
  val addrDecode       = Module(new AddrDecode(addrDecodeParams))
  addrDecode.io.addr       := io.apb.PADDR
  addrDecode.io.addrOffset := 0.U
  addrDecode.io.en         := true.B
  addrDecode.io.selInput   := true.B

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
  // Master Clock Divider
  // ------------------------------------------------------------------------
  val sclCnt       = RegInit(0.U(16.W))
  val sclToggleReg = RegInit(true.B)
  when(sclCnt === 0.U) {
    sclCnt       := mbaud
    sclToggleReg := ~sclToggleReg
  } .otherwise {
    sclCnt := sclCnt - 1.U
  }

  // ------------------------------------------------------------------------
  // Master FSM (Falling-Edge Shifting)
  // ------------------------------------------------------------------------
  val idleMaster           = 0.U
  val startConditionMaster = 1.U
  val sendAddrMaster       = 2.U
  val waitAckAddrMaster    = 3.U
  val txDataMaster         = 4.U
  val waitAckDataMaster    = 5.U
  val rxDataMaster         = 6.U
  val waitAckRxMaster      = 7.U
  val stopConditionMaster  = 8.U

  val masterFSM    = RegInit(idleMaster)
  val sclMasterReg = RegInit(true.B)
  val sdaMasterReg = RegInit(true.B)
  val bitCnt       = RegInit(0.U(4.W))

  val addrWithRW   = RegInit(0.U(8.W))
  val rxShift      = RegInit(0.U(p.dataWidth.W))
  val txShift      = RegInit(0.U(p.dataWidth.W))

  switch(masterFSM) {
    is(idleMaster) {
      sclMasterReg := true.B
      sdaMasterReg := true.B
      bitCnt := 0.U
      mstatus := 0.U
      when(mctrla(0)) {
        masterFSM := startConditionMaster
      }
    }

    is(startConditionMaster) {
      // drive SDA low first, while SCL=high
      when(sclMasterReg && sdaMasterReg) {
        sdaMasterReg := false.B
      }
      .elsewhen(!sdaMasterReg && sclMasterReg) {
        sclMasterReg := false.B
        masterFSM := sendAddrMaster
        bitCnt := 0.U
      }
    }

    is(sendAddrMaster) {
      // 7-bit + R/W => 8 bits
      val slave7 = maddr(7,1)
      val rwBit  = maddr(0)
      addrWithRW := Cat(slave7, rwBit)

      // SHIFT OUT on FALLING edges
      when(!sclMasterReg && sclToggleReg) {
        val outBit = addrWithRW(7.U - bitCnt)
        sdaMasterReg := outBit
        printf(p"[Master] sendAddr bit=$outBit bitCnt=$bitCnt\n")
        bitCnt := bitCnt + 1.U
      }
      // toggle SCL
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      when(bitCnt===8.U) {
        masterFSM := waitAckAddrMaster
        bitCnt := 0.U
      }
    }

    is(waitAckAddrMaster) {
      // release sda => high
      when(!sclMasterReg && sclToggleReg) {
        sdaMasterReg := true.B
      }
      // toggle scl
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      // on next falling => read ack => ignore => pick TX or RX
      when(!sclMasterReg && sclToggleReg) {
        val rwBit = addrWithRW(0)
        masterFSM := Mux(rwBit===1.U, rxDataMaster, txDataMaster)
      }
    }

    is(txDataMaster) {
      txShift := mdata
      when(!sclMasterReg && sclToggleReg) {
        val outBit = txShift(7.U - bitCnt)
        sdaMasterReg := outBit
        printf(p"[Master] TX bit=$outBit bitCnt=$bitCnt\n")
        bitCnt := bitCnt + 1.U
      }
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      when(bitCnt===8.U) {
        masterFSM := waitAckDataMaster
        bitCnt := 0.U
      }
    }

    is(waitAckDataMaster) {
      when(!sclMasterReg && sclToggleReg) {
        sdaMasterReg := true.B
      }
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      when(!sclMasterReg && sclToggleReg) {
        masterFSM := stopConditionMaster
      }
    }

    is(rxDataMaster) {
      // SHIFT IN on FALLING edges
      when(!sclMasterReg && sclToggleReg) {
        val inBit = io.sdaIn & io.sdaOut
        rxShift := Cat(rxShift(p.dataWidth-2,0), inBit)
        printf(p"[MasterRx] bit=$inBit bitCnt=$bitCnt => shift=${Binary(rxShift)}\n")
        bitCnt := bitCnt + 1.U
      }
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      when(bitCnt===8.U) {
        mdata := rxShift
        mstatus := 2.U
        printf(p"[MasterRx] final=0x${Hexadecimal(rxShift)}\n")
        masterFSM := waitAckRxMaster
        bitCnt := 0.U
      }
    }

    is(waitAckRxMaster) {
      // master ack => sda=0
      when(!sclMasterReg && sclToggleReg) {
        sdaMasterReg := false.B
      }
      when(sclToggleReg) {
        sclMasterReg := !sclMasterReg
      }
      when(!sclMasterReg && sclToggleReg) {
        masterFSM := stopConditionMaster
      }
    }

    is(stopConditionMaster) {
      // release
      when(!sclMasterReg && sclToggleReg) {
        sclMasterReg := true.B
      }
      .elsewhen(sclMasterReg && !sdaMasterReg && sclToggleReg) {
        sdaMasterReg := true.B
      }
      .elsewhen(sclMasterReg && sdaMasterReg) {
        masterFSM := idleMaster
      }
    }
  }

  // ------------------------------------------------------------------------
  // Slave FSM (Falling-Edge Sample, Always ACK)
  // ------------------------------------------------------------------------
  val idleSlave     = 0.U
  val startDetected = 1.U
  val addrReceive   = 2.U
  val addrAck       = 3.U
  val dataReceive   = 4.U
  val dataAck       = 5.U

  val slaveFSM     = RegInit(idleSlave)
  val sclSlaveReg  = RegInit(true.B)
  val sdaSlaveReg  = RegInit(true.B)
  val sBitCnt      = RegInit(0.U(4.W))
  val sShift       = RegInit(0.U(p.dataWidth.W))
  val slaveEn      = sctrla(0)

  switch(slaveFSM) {
    is(idleSlave) {
      sstatus := 0.U
      sclSlaveReg := true.B
      sdaSlaveReg := true.B
      // if enabled & sdaIn=0 while sclIn=1 => start
      when(slaveEn && !io.sdaIn && io.sclIn) {
        slaveFSM := startDetected
        printf("[Slave] start detected\n")
      }
    }

    is(startDetected) {
      sBitCnt := 0.U
      sShift := 0.U
      slaveFSM := addrReceive
    }

    is(addrReceive) {
      // SHIFT on FALLING => if(!io.sclIn)
      when(!io.sclIn) {
        val inBit = io.sdaIn & io.sdaOut
        sShift := Cat(sShift(p.dataWidth-2,0), inBit)
        printf(p"[SlaveAddr] bit=$inBit bitCnt=$sBitCnt => shift=${Binary(sShift)}\n")
        sBitCnt := sBitCnt + 1.U
      }
      when(sBitCnt===7.U && !io.sclIn) {
        slaveFSM := addrAck
      }
    }

    is(addrAck) {
      sstatus := 1.U
      sdaSlaveReg := false.B
      printf(p"[SlaveAddr] final=0x${Hexadecimal(sShift)}, ack\n")
      sBitCnt := 0.U
      sShift := 0.U
      slaveFSM := dataReceive
    }

    is(dataReceive) {
      // SHIFT on FALLING
      when(!io.sclIn) {
        val inBit = io.sdaIn & io.sdaOut
        sShift := Cat(sShift(p.dataWidth-2,0), inBit)
        printf(p"[SlaveData] bit=$inBit bitCnt=$sBitCnt => shift=${Binary(sShift)}\n")
        sBitCnt := sBitCnt + 1.U
      }
      when(sBitCnt===7.U && !io.sclIn) {
        slaveFSM := dataAck
      }
    }

    is(dataAck) {
      sdata := sShift
      sstatus := 2.U
      sdaSlaveReg := false.B
      printf(p"[SlaveData] final=0x${Hexadecimal(sShift)}, ack\n")
      sBitCnt := 0.U
      sShift := 0.U
      slaveFSM := dataReceive
    }
  }

  // ------------------------------------------------------------------------
  // Combine Master + Slave => wired-AND
  // ------------------------------------------------------------------------
  io.sclOut := sclMasterReg & sclSlaveReg
  io.sdaOut := sdaMasterReg & sdaSlaveReg

  io.interrupt := (mstatus===2.U) || (sstatus===2.U)
}
