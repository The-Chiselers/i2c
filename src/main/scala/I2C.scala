package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap


/**
  * Main I2C module with:
  *   - APB register map
  *   - Master + Slave regs
  *   - Master SCL clock using Dividers
  *   - Plain UInt state machine for Master/Slave
  */
class I2C(p: BaseParams) extends Module {
  val io = IO(new Bundle {
    val apb       = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val master    = new MasterInterface
    val slave     = new SlaveInterface
    val interrupt = Output(Bool())
  })

  // ------------------------------------------------------------------------
  // 1) Register Map
  // ------------------------------------------------------------------------
  val registerMap = new RegisterMap(p.regWidth, p.addrWidth)

  // Internal signals
  val maddrFlag    = RegInit(false.B)
  val i2cShift     = RegInit(0.U(p.dataWidth.W))
  val addrShift    = RegInit(0.U(8.W))
  val shiftCounter = RegInit(0.U((log2Ceil(p.dataWidth) + 1).W))
  val frameCounter = RegInit(0.U(5.W))

  // MASTER REGISTERS
  val mctrla  = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mctrla,  "mctrla")

  val mctrlb  = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mctrlb,  "mctrlb")

  val mstatus = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mstatus, "mstatus")

  // We set mbaud = 10 by default to avoid 0 => huge period
  val mbaud   = RegInit(10.U(p.regWidth.W))
  registerMap.createAddressableRegister(mbaud,   "mbaud")

  val maddr   = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(maddr,   "maddr")

  val mdata   = RegInit(0.U(p.dataWidth.W))
  registerMap.createAddressableRegister(mdata,   "mdata")

  // SLAVE REGISTERS
  val sctrla    = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sctrla,    "sctrla")

  val sctrlb    = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sctrlb,    "sctrlb")

  val sstatus   = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sstatus,   "sstatus")

  val saddr     = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(saddr,     "saddr")

  val sdata     = RegInit(0.U(p.dataWidth.W))
  registerMap.createAddressableRegister(sdata,     "sdata")

  val saddrmask = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(saddrmask, "saddrmask")

  // ------------------------------------------------------------------------
  // 2) AddrDecode
  // ------------------------------------------------------------------------
  val addrDecodeParams = registerMap.getAddrDecodeParams
  val addrDecode       = Module(new AddrDecode(addrDecodeParams))
  addrDecode.io.addr       := io.apb.PADDR
  addrDecode.io.addrOffset := 0.U
  addrDecode.io.en         := true.B
  addrDecode.io.selInput   := true.B

  // ------------------------------------------------------------------------
  // 3) APB Interface
  // ------------------------------------------------------------------------
  io.apb.PREADY  := (io.apb.PENABLE && io.apb.PSEL)
  io.apb.PSLVERR := (addrDecode.io.errorCode === AddrDecodeError.AddressOutOfRange)
  io.apb.PRDATA  := 0.U

  // APB Read/write
  when(io.apb.PSEL && io.apb.PENABLE) {
    when(io.apb.PWRITE) {
      // Write
      for (reg <- registerMap.getRegisters) {
        when(addrDecode.io.sel(reg.id)) {
          if (reg.name == "mdata" || reg.name == "sdata") {
            i2cShift := io.apb.PWDATA
          } else {
            reg.writeCallback(addrDecode.io.addrOffset, io.apb.PWDATA)
          }
          // If the user wrote MADDR => we set a flag
          if (reg.name == "maddr") {
            maddrFlag := true.B
          }
        }
      }
    } .otherwise {
      // Read
      for (reg <- registerMap.getRegisters) {
        when(addrDecode.io.sel(reg.id)) {
          if (reg.name == "mdata" || reg.name == "sdata") {
            io.apb.PRDATA := i2cShift
          } else {
            io.apb.PRDATA := reg.readCallback(addrDecode.io.addrOffset)
          }
        }
      }
    }
  }

  // ------------------------------------------------------------------------
  // 4) Divider-based Master Clock
  // ------------------------------------------------------------------------
  val dividerFreq = Module(new Divider())
  dividerFreq.io.numerator   := (p.clkFreq.U * 1000000.U)
  dividerFreq.io.denominator := (10.U + (2.U * mbaud))
  dividerFreq.io.start       := false.B

  val dividerPer = Module(new Divider())
  dividerPer.io.numerator   := (p.clkFreq.U * 1000000.U)
  dividerPer.io.denominator := (2.U * dividerFreq.io.result)
  dividerPer.io.start       := false.B

  val freqReg       = RegInit(1.U(32.W))
  val halfPeriodReg = RegInit(1000.U(32.W))

  // Simple 3-state FSM for the two-step division
  val DIV_IDLE = 0.U
  val DIV_FREQ = 1.U
  val DIV_PER  = 2.U
  val divState = RegInit(DIV_IDLE)

  val lastMbaud    = RegNext(mbaud)
  val mbaudChanged = RegInit(false.B)
  when(mbaud =/= lastMbaud) {
    mbaudChanged := true.B
  }.otherwise {
    mbaudChanged := false.B
  }

  val masterEn     = mctrla(0)
  val lastMasterEn = RegNext(masterEn, false.B)
  val justEnabled  = masterEn && !lastMasterEn

  // Debug prints
  // printf(p"mbaud: $mbaud, mctrla: $mctrla\n")
  // printf(p"divState: $divState, freqReg: $freqReg, halfPeriodReg: $halfPeriodReg\n")

  // Inside the FSM logic in I2C.scala

  switch(divState) {
    is(DIV_IDLE) {
      when(masterEn && (justEnabled || mbaudChanged)) {
        printf(p"Transitioning from DIV_IDLE to DIV_FREQ: justEnabled=${justEnabled}, mbaud_changed=${mbaudChanged}\n")
        dividerFreq.io.start := true.B
        divState := DIV_FREQ
      }
    }
    is(DIV_FREQ) {
      when(dividerFreq.io.valid) {
        printf(p"DividerFreq completed: result=${dividerFreq.io.result}\n")
        freqReg := dividerFreq.io.result
        dividerPer.io.numerator   := (p.clkFreq.U * 1000000.U)
        dividerPer.io.denominator := (2.U * dividerFreq.io.result)
        dividerPer.io.start       := true.B
        divState := DIV_PER
      } .otherwise {
        dividerFreq.io.start := false.B
      }
    }
    is(DIV_PER) {
      when(dividerPer.io.valid) {
        printf(p"DividerPer completed: result=${dividerPer.io.result}\n")
        halfPeriodReg := dividerPer.io.result
        divState := DIV_IDLE
      } .otherwise {
        dividerPer.io.start := false.B
      }
    }
  }

  // SCL toggling
  val sclCounter = RegInit(0.U(32.W))
  val sclReg     = RegInit(true.B)
  io.master.scl  := true.B

  when(masterEn) {
    when(sclCounter >= (halfPeriodReg - 1.U)) {
      sclCounter := 0.U
      sclReg     := ~sclReg
    } .otherwise {
      sclCounter := sclCounter + 1.U
    }
    io.master.scl := sclReg
  }

  // Debug prints for SCL
  // printf(p"sclCounter: $sclCounter, sclReg: $sclReg\n")

  // ------------------------------------------------------------------------
  // 5) Master/Slave FSM Using UInt States
  // ------------------------------------------------------------------------
  val STATE_IDLE          = 0.U
  val STATE_MASTERADDRESS = 1.U
  val STATE_SLAVEADDRESS  = 2.U
  val STATE_WAITACKMASTER = 3.U
  val STATE_WAITACKSLAVE  = 4.U
  val STATE_SENDACKMASTER = 5.U
  val STATE_SENDACKSLAVE  = 6.U
  val STATE_MASTERWRITE   = 7.U
  val STATE_SLAVEWRITE    = 8.U
  val STATE_MASTERREAD    = 9.U
  val STATE_SLAVEREAD     = 10.U
  val STATE_SENDSTOP      = 11.U
  val STATE_WAITSTOP      = 12.U

  val stateReg = RegInit(STATE_IDLE)

  // Local regs for the FSM
  val rwBit   = RegInit(0.U(1.W))
  val ssFlag  = RegInit(0.U(1.W))
  val prevSda = RegInit(0.U(1.W))
  val prevClk = RegInit(true.B)

  // Default signals
  io.interrupt     := false.B
  io.master.sdaOut := true.B
  io.slave.sdaOut  := true.B

  switch(stateReg) {
    // IDLE
    is(STATE_IDLE) {
      shiftCounter := 0.U
      frameCounter := 0.U

      // Only do a one-time action if *just* entered IDLE from another state, e.g.:
      // when(previousState =/= STATE_IDLE) { sclReg := true.B }

      // Or simply do nothing with sclReg at all:
      //   Let the toggling logic handle sclReg.
      //   If you want SCL to remain high in IDLE, do it only if masterEn = false:
      when(!masterEn) { sclReg := true.B }
      // MASTER
      when(mctrla(0) === 1.U) {
        when(maddrFlag) {
          io.master.sdaOut := 0.U
          ssFlag := 1.U
          stateReg := STATE_IDLE
          when(ssFlag === 1.U) {
            maddrFlag := false.B
            ssFlag    := 0.U
            sclReg    := false.B
            addrShift := maddr
            stateReg  := STATE_MASTERADDRESS
          }
        }
      }

      // SLAVE
      when(sctrla(0) === 1.U) {
        when(io.slave.sdaIn === 0.U) {
          ssFlag   := 1.U
          stateReg := STATE_IDLE
        }
        when(ssFlag === 1.U) {
          when(!io.slave.scl) {
            ssFlag   := 0.U
            stateReg := STATE_SLAVEADDRESS
          }
        }
      }
    }

    // MASTER ADDRESS SHIFT
    is(STATE_MASTERADDRESS) {
      io.master.sdaOut := addrShift(p.dataWidth - 1)
      prevClk := sclReg
      when(~prevClk & io.master.scl) {
        when(frameCounter < 7.U) {
          addrShift   := addrShift(p.dataWidth - 2, 0) ## 0.U
          frameCounter := frameCounter + 1.U
          stateReg    := STATE_MASTERADDRESS
        }.otherwise {
          rwBit := addrShift(p.dataWidth - 1)
          stateReg := STATE_WAITACKMASTER
        }
      }
    }

    // SLAVE ADDRESS SHIFT
    is(STATE_SLAVEADDRESS) {
      prevClk := io.slave.scl
      when(~prevClk & io.slave.scl) {
        when(frameCounter < 7.U) {
          addrShift   := addrShift(p.dataWidth - 2, 0) ## io.slave.sdaIn
          frameCounter := frameCounter + 1.U
          stateReg    := STATE_SLAVEADDRESS
        }.otherwise {
          rwBit := io.slave.sdaIn
          // Compare with saddr => ack if match
          when(saddr === addrShift) {
            stateReg := STATE_SENDACKSLAVE
          } .otherwise {
            stateReg := STATE_IDLE
          }
        }
      }
    }

    // WAIT ACK MASTER
    is(STATE_WAITACKMASTER) {
      prevClk := sclReg
      prevSda := io.master.sdaIn
      frameCounter := 0.U
      when(~prevClk & io.master.scl) {
        when((prevSda === 1.U) && (io.master.sdaIn === 0.U)) {
          // got ACK
          stateReg := Mux(rwBit === 1.U, STATE_MASTERWRITE, STATE_MASTERREAD)
        }
      }
    }

    // WAIT ACK SLAVE
    is(STATE_WAITACKSLAVE) {
      prevClk := io.slave.scl
      prevSda := io.slave.sdaIn
      frameCounter := 0.U
      when(~prevClk & io.slave.scl) {
        when((prevSda === 1.U) && (io.slave.sdaIn === 0.U)) {
          stateReg := Mux(rwBit === 1.U, STATE_SLAVEREAD, STATE_SLAVEWRITE)
        }
      }
    }

    // SEND ACK SLAVE
    is(STATE_SENDACKSLAVE) {
      prevClk := io.slave.scl
      io.slave.sdaOut := 0.U
      frameCounter := 0.U
      when(~prevClk & io.slave.scl) {
        stateReg := Mux(rwBit === 1.U, STATE_SLAVEREAD, STATE_SLAVEWRITE)
      }
    }

    // SEND ACK MASTER
    is(STATE_SENDACKMASTER) {
      prevClk := sclReg
      io.master.sdaOut := 0.U
      frameCounter := 0.U
      when(~prevClk & io.master.scl) {
        stateReg := Mux(rwBit === 1.U, STATE_MASTERWRITE, STATE_MASTERREAD)
      }
    }

    // MASTER WRITE
    is(STATE_MASTERWRITE) {
      io.master.sdaOut := i2cShift(p.dataWidth - 1)
      prevClk := sclReg
      when(~prevClk & io.master.scl) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## 0.U
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_MASTERWRITE
        }.otherwise {
          when(shiftCounter === p.dataWidth.U) {
            sclReg := true.B
            stateReg := STATE_SENDSTOP
          } .otherwise {
            stateReg := STATE_WAITACKMASTER
          }
        }
      }
    }

    // SLAVE WRITE
    is(STATE_SLAVEWRITE) {
      io.slave.sdaOut := i2cShift(p.dataWidth - 1)
      prevClk := io.slave.scl
      when(~prevClk & io.slave.scl) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## 0.U
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_SLAVEWRITE
        }.otherwise {
          when(shiftCounter === p.dataWidth.U) {
            stateReg := STATE_WAITSTOP
          } .otherwise {
            stateReg := STATE_WAITACKSLAVE
          }
        }
      }
    }

    // MASTER READ
    is(STATE_MASTERREAD) {
      prevClk := sclReg
      when(~prevClk & io.master.scl) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## io.master.sdaIn
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_MASTERREAD
        }.otherwise {
          when(shiftCounter === p.dataWidth.U) {
            sclReg := true.B
            stateReg := STATE_SENDSTOP
          } .otherwise {
            stateReg := STATE_SENDACKMASTER
          }
        }
      }
    }

    // SLAVE READ
    is(STATE_SLAVEREAD) {
      prevClk := io.slave.scl
      when(~prevClk & io.slave.scl) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## io.slave.sdaIn
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_SLAVEREAD
        }.otherwise {
          when(shiftCounter === p.dataWidth.U) {
            stateReg := STATE_WAITSTOP
          } .otherwise {
            stateReg := STATE_SENDACKSLAVE
          }
        }
      }
    }

    // MASTER SEND STOP
    is(STATE_SENDSTOP) {
      io.master.sdaOut := 1.U
      sclReg := true.B
      stateReg := STATE_IDLE
    }

    // SLAVE WAIT STOP
    is(STATE_WAITSTOP) {
      when(io.slave.scl) {
        ssFlag := 1.U
        stateReg := STATE_WAITSTOP
      }
      when(ssFlag === 1.U) {
        when(io.slave.sdaIn === 1.U) {
          ssFlag := 0.U
          stateReg := STATE_IDLE
        }
      }
    }
  }
}
