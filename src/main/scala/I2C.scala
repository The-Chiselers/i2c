
package tech.rocksavage.chiselware.I2C
import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap
/**
  * I2C module with:
  *   - APB register map
  *   - Master + Slave registers
  *   - Master SCL clock using Dividers
  *   - FSM for Master/Slave
  *
  * Debug prints only on major FSM transitions for readability.
  *
  * STOP-Condition logic:
  *   - Slave sets AP=1 (bit0 of sstatus) when a valid address is recognized.
  *   - Then, if it sees STOP (SCL=1, SDA=1) in WAITSTOP, it sets APIF=1 (bit6) and AP=0.
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
  val i2cShift     = RegInit(0.U(p.dataWidth.W)) // shared shift register
  val addrShift    = RegInit(0.U(8.W))           // for address bits
  val shiftCounter = RegInit(0.U((log2Ceil(p.dataWidth) + 1).W))
  val frameCounter = RegInit(0.U(5.W))
  // MASTER REGISTERS
  val mctrla  = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mctrla, "mctrla")
  val mctrlb  = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mctrlb, "mctrlb")
  val mstatus = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mstatus, "mstatus")
  // Default mbaud=10 to avoid divide-by-zero if never written
  val mbaud   = RegInit(10.U(p.regWidth.W))
  registerMap.createAddressableRegister(mbaud, "mbaud")
  val maddr   = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(maddr, "maddr")
  val mdata   = RegInit(0.U(p.dataWidth.W))
  registerMap.createAddressableRegister(mdata, "mdata")
  // SLAVE REGISTERS
  val sctrla    = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sctrla, "sctrla")
  val sctrlb    = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sctrlb, "sctrlb")
  val sstatus   = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sstatus, "sstatus")
  val saddr     = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(saddr, "saddr")
  val sdata     = RegInit(0.U(p.dataWidth.W))
  registerMap.createAddressableRegister(sdata, "sdata")
  val saddrmask = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(saddrmask, "saddrmask")
  // ------------------------------------------------------------------------
  // 2) AddrDecode
  // ------------------------------------------------------------------------
  val addrDecodeParams = registerMap.getAddrDecodeParams
  val addrDecode       = Module(new AddrDecode(addrDecodeParams))
  addrDecode.io.addr      := io.apb.PADDR
//  addrDecode.io.addrOffset := 0.U
  addrDecode.io.en         := true.B
  addrDecode.io.selInput   := true.B
  // ------------------------------------------------------------------------
  // 3) APB Interface
  // ------------------------------------------------------------------------
  io.apb.PREADY  := (io.apb.PENABLE && io.apb.PSEL)
  io.apb.PSLVERR := (addrDecode.io.errorCode === AddrDecodeError.AddressOutOfRange)
  io.apb.PRDATA  := 0.U
  when(io.apb.PSEL && io.apb.PENABLE) {
    when(io.apb.PWRITE) {
      // WRITE to a register
      for (reg <- registerMap.getRegisters) {
        when(addrDecode.io.sel(reg.id)) {
          if(reg.name == "mdata"){
            i2cShift := io.apb.PWDATA
            mstatus := mstatus & "b00111111".U //Clear interrupts
          }
          if (reg.name == "sdata"){
            i2cShift := io.apb.PWDATA
            sstatus := sstatus & "b01111111".U //Clear interrupts
          }
          if (reg.name == "maddr"){
            maddrFlag := true.B
            mstatus := mstatus & "b00111111".U //Clear interrupts
          }
          if(reg.name == "mstatus"){
            mstatus := mstatus & ~io.apb.PWDATA
          }
          if (reg.name == "sstatus"){
            sstatus := sstatus & ~io.apb.PWDATA
          }
          reg.writeCallback(addrDecode.io.addrOut, io.apb.PWDATA)            
        }
      }        
    } .otherwise {
      // READ from a register
      for (reg <- registerMap.getRegisters) {
        when(addrDecode.io.sel(reg.id)) {
          if (reg.name == "mdata" || reg.name == "sdata") {
            io.apb.PRDATA := i2cShift
          } else {
            io.apb.PRDATA := reg.readCallback(addrDecode.io.addrOut)
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
  val DIV_IDLE = 0.U
  val DIV_FREQ = 1.U
  val DIV_PER  = 2.U
  val divState = RegInit(DIV_IDLE)
  val lastMbaud    = RegNext(mbaud)
  val mbaudChanged = RegInit(false.B)
  when(mbaud =/= lastMbaud) {
    mbaudChanged := true.B
  } .otherwise {
    mbaudChanged := false.B
  }
  val masterEn     = mctrla(0) // bit0 => enable master
  val lastMasterEn = RegNext(masterEn, false.B)
  val justEnabled  = masterEn && !lastMasterEn
  switch(divState) {
    is(DIV_IDLE) {
      when(masterEn && (justEnabled || mbaudChanged)) {
        printf(p"[I2C] Transitioning from DIV_IDLE to DIV_FREQ: justEnabled=$justEnabled, mbaud_changed=$mbaudChanged\n")
        dividerFreq.io.start := true.B
        divState := DIV_FREQ
      }
    }
    is(DIV_FREQ) {
      when(dividerFreq.io.valid) {
        printf(p"[I2C] DividerFreq completed: result=${dividerFreq.io.result}\n")
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
        printf(p"[I2C] DividerPer completed: result=${dividerPer.io.result}\n")
        halfPeriodReg := dividerPer.io.result
        divState := DIV_IDLE
      } .otherwise {
        dividerPer.io.start := false.B
      }
    }
  }
  // SCL toggling for the master (only active when master is enabled)
  val sclCounter = RegInit(0.U(32.W))
  val sclReg     = RegInit(true.B)
  io.master.scl := true.B
  when(masterEn) {
    when(sclCounter >= (halfPeriodReg - 1.U)) {
      sclCounter := 0.U
      sclReg     := ~sclReg
    } .otherwise {
      sclCounter := sclCounter + 1.U
    }
    io.master.scl := sclReg
  }

  // ------------------------------------------------------------------------
  // 5) Master/Slave FSM Using UInt States
  // ------------------------------------------------------------------------

  object BusState extends ChiselEnum {
    val IDLE, BUSY, OWNER = Value
  }  
  val busStateReg = RegInit(BusState.IDLE)
  val prevSdaBus = RegInit(0.U(1.W))
  val prevClkBus = RegInit(true.B)
  val ssFlag  = RegInit(0.U(1.W))
  /*
  switch(busStateReg) {
    is(BusState.IDLE) {
      when(mctrla(0) === 1.U && maddrFlag) { //Internal START Condition
        busStateReg := BusState.OWNER
      }.elsewhen(detectStartConditionMaster()){ //External START Condition
        busStateReg := BusState.BUSY
      }
    }
    is(BusState.BUSY) {
      when(io.master.scl && io.master.sdaIn === 1.U) { //STOP Condition Detected
        busStateReg := BusState.IDLE
      }
    }
    is(BusState.OWNER) {
      when((io.master.scl && io.master.sdaIn === 1.U)) { //Need to add check for arb lost
        busStateReg := BusState.IDLE
      }
    }
  }
    */

  // ------------------------------------------------------------------------
  // 6) Master/Slave FSM Using UInt States
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
  val stateReg      = RegInit(STATE_IDLE)
  val previousState = RegInit(STATE_IDLE)
  // Local registers for the FSM
  val rwBit   = RegInit(0.U(1.W))
  val prevSda = RegInit(0.U(1.W))
  val prevClk = RegInit(true.B)
  // Default outputs for SDA lines
  io.interrupt     := false.B
  io.master.sdaOut := true.B
  io.slave.sdaOut  := true.B
  // Debug print for state transitions
  def printDebugTransition(oldSt: UInt, newSt: UInt): Unit = {
    printf(p"[FSM TRANS] $oldSt => $newSt: rw=$rwBit shiftCtr=$shiftCounter frmCtr=$frameCounter i2cShift=0x${Hexadecimal(i2cShift)} mdata=0x${Hexadecimal(mdata)} sdata=0x${Hexadecimal(sdata)} scl=$sclReg masterEn=$masterEn\n")
  }
  when(stateReg =/= previousState) {
    printDebugTransition(previousState, stateReg)
  }
  previousState := stateReg

  switch(stateReg) {
    // -------------------------------------------------------
    // IDLE
    // -------------------------------------------------------
    is(STATE_IDLE) {
      shiftCounter := 0.U
      frameCounter := 0.U
      when(!masterEn) { sclReg := true.B }
      
      // MASTER side: When enabled and address written, drive SDA low and prepare to send address.
      when(mctrla(0) === 1.U && maddrFlag) {
        sendStartCondition()
      }
      
      // SLAVE side: If enabled, look for SDA=0 => potential start
      when(sctrla(0) === 1.U) {
        when(detectStartConditionSlave()) {
          stateReg := STATE_SLAVEADDRESS
        }
      }
    }
    
    // -------------------------------------------------------
    // MASTERADDRESS
    // -------------------------------------------------------
    is(STATE_MASTERADDRESS) {
      io.master.sdaOut := addrShift(p.dataWidth - 1)
      prevClk := io.master.scl
      when(~prevClk & io.master.scl) {
        when(frameCounter < 7.U) {
          addrShift   := addrShift(p.dataWidth - 2, 0) ## 0.U
          frameCounter := frameCounter + 1.U
          stateReg    := STATE_MASTERADDRESS
        } .otherwise {
          rwBit := addrShift(p.dataWidth - 1) // LSB is R/W bit
          prevSda := io.master.sdaIn
          stateReg := STATE_WAITACKMASTER
        }
      }
    }
    
    // -------------------------------------------------------
    // SLAVEADDRESS
    // -------------------------------------------------------
    is(STATE_SLAVEADDRESS) {
      prevClk := io.slave.scl
      when(~prevClk & io.slave.scl) {
        when(frameCounter < 7.U) {
          addrShift    := addrShift(p.dataWidth - 2, 0) ## io.slave.sdaIn
          frameCounter := frameCounter + 1.U
          stateReg     := STATE_SLAVEADDRESS
        } .otherwise {
          rwBit := io.slave.sdaIn
          // If address matches, set AP=1 in sstatus, then do SENDACKSLAVE
          when(saddr === addrShift) {
            sstatus := sstatus | 1.U
            stateReg := STATE_SENDACKSLAVE
          } .otherwise {
            stateReg := STATE_IDLE
          }
        }
      }
    }
    
    // -------------------------------------------------------
    // WAITACKMASTER
    // -------------------------------------------------------
    is(STATE_WAITACKMASTER) {
      prevClk := io.master.scl
      frameCounter := 0.U
      when(~prevClk & io.master.scl) {
        prevSda := io.master.sdaIn
        when((prevSda === 1.U) && (io.master.sdaIn === 0.U)) {
          when(rwBit === 0.U) {
            /* 
            *  Master Case 1: Address Packet Transmit Complete - Direction Bit Set to Write
            *  When a slave device responds to the address packet with an ACK, do the following:
            *  Set Write Interrupt Flag (WIF = 1)
            *  Clear Recieved Acknowledge (RXACK = 0)
            *  Set Clock Hold (CLKHOLD = 1)
            *  Then, prepare to transmit data to slave via STATE_MASTERWRTE
            */
            mstatus := mstatus | (1.U << 6)  // WIF = 1
            mstatus := mstatus & ~(1.U << 4) // RXACK = 0
            mstatus := mstatus | (1.U << 5)  // CLKHOLD = 1
            sclReg := false.B  // Hold SCL low

            i2cShift     := mdata
            shiftCounter := 0.U
            frameCounter := 0.U
            stateReg     := STATE_MASTERWRITE
          } .otherwise {
            /* 
            *  Master Case 2: Address Packet Transmit Complete - Direction Bit Set to Read
            *  When a slave device responds to the address packet with an ACK, do the following:
            *  Clear Recieved Acknowledge (RXACK = 0)
            *  Set Clock Hold (CLKHOLD = 1)
            *  Then, prepare to read data from slave via STATE_MASTERREAD
            */
            mstatus := mstatus & ~(1.U << 4) // RXACK = 0
            mstatus := mstatus | (1.U << 5)  // CLKHOLD = 1
            sclReg := false.B  // Hold SCL low

            i2cShift     := 0.U
            shiftCounter := 0.U
            frameCounter := 0.U
            stateReg     := STATE_MASTERREAD
          }
        } .otherwise {
            /* 
            *  Master Case 3: Address Packet Transmit Complete - NACK from Slave
            *  When no slave device responds to the address packet, do the following:
            *  Set Write Interrupt Flag (WIF = 1) 
            *  Set Recieved Acknowledge (RXACK = 1)
            *  Then, issue a STOP condition to end the transaction
            */
            mstatus := mstatus | (1.U << 6)  // WIF = 1
            mstatus := mstatus | (1.U << 4)  // RXACK = 1
            stateReg := STATE_SENDSTOP
        }
      }
    }
    
    // -------------------------------------------------------
    // WAITACKSLAVE
    // -------------------------------------------------------
    is(STATE_WAITACKSLAVE) {
      prevClk := io.slave.scl
      frameCounter := 0.U
      when(~prevClk & io.slave.scl) {
        prevSda := io.slave.sdaIn
        when((prevSda === 1.U) && (io.slave.sdaIn === 0.U)) {
          when(rwBit === 1.U) {
            i2cShift    := sdata
            shiftCounter := 0.U
            frameCounter := 0.U
            stateReg     := STATE_SLAVEWRITE
          } .otherwise {
            i2cShift    := 0.U
            shiftCounter := 0.U
            frameCounter := 0.U
            stateReg     := STATE_SLAVEREAD
          }
        } .otherwise {
          // No ACK => assume NACK => go to WAITSTOP
          stateReg := STATE_WAITSTOP
        }
      }
    }
    
    // -------------------------------------------------------
    // SENDACKSLAVE
    // -------------------------------------------------------
    is(STATE_SENDACKSLAVE) {
      prevClk := io.slave.scl
      io.slave.sdaOut := 0.U
      frameCounter := 0.U
      when(~prevClk & io.slave.scl) {
        stateReg := Mux(rwBit === 1.U, STATE_SLAVEWRITE, STATE_SLAVEREAD)
      }
    }
    
    // -------------------------------------------------------
    // SENDACKMASTER
    // -------------------------------------------------------
    is(STATE_SENDACKMASTER) {
      prevClk := io.master.scl
      io.master.sdaOut := 0.U
      frameCounter := 0.U
      when(~prevClk & io.master.scl) {
        stateReg := Mux(rwBit === 0.U, STATE_MASTERWRITE, STATE_MASTERREAD)
      }
    }
    
    // -------------------------------------------------------
    // MASTERWRITE
    // -------------------------------------------------------
    is(STATE_MASTERWRITE) {
      io.master.sdaOut := i2cShift(p.dataWidth - 1)
      prevClk := io.master.scl
      when(~prevClk & io.master.scl) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## 0.U
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_MASTERWRITE
        } .otherwise {
          when(shiftCounter === p.dataWidth.U) {
            sclReg := true.B
            when(mctrla(6) === 1.U){  //When master write complete interrupts are enabled
              mstatus := mstatus | (1.U << 6.U) //Write master write complete interrupt to register
            }
            stateReg := STATE_SENDSTOP
          } .otherwise {
            prevSda  := io.master.sdaIn
            stateReg := STATE_WAITACKMASTER
          }
        }
      }
    }
    
    // -------------------------------------------------------
    // SLAVEWRITE
    // -------------------------------------------------------
    is(STATE_SLAVEWRITE) {
      io.slave.sdaOut := i2cShift(p.dataWidth - 1)
      prevClk := io.slave.scl
      when(~prevClk & io.slave.scl) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## 0.U
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_SLAVEWRITE
        } .otherwise {
          when(shiftCounter === p.dataWidth.U) {
            when(sctrla(7) === 1.U) {
              sstatus := sstatus | (1.U << 7.U)
            }
            stateReg := STATE_WAITSTOP
          } .otherwise {
            prevSda := io.slave.sdaIn
            stateReg := STATE_WAITACKSLAVE
          }
        }
      }
    }
    
    // -------------------------------------------------------
    // MASTERREAD
    // -------------------------------------------------------
    is(STATE_MASTERREAD) {
      prevClk := io.master.scl
      when(~prevClk & io.master.scl) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## io.master.sdaIn
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_MASTERREAD
        } .otherwise {
          when(shiftCounter === p.dataWidth.U) {
            mdata := i2cShift
            sclReg := true.B
            when(mctrla(7) === 1.U){  //When master read complete interrupts are enabled
              mstatus := mstatus | (1.U << 7.U) //Write master read complete interrupt to register
            }
            stateReg := STATE_SENDSTOP
          } .otherwise {
            stateReg := STATE_SENDACKMASTER
          }
        }
      }
    }
    
    // -------------------------------------------------------
    // SLAVEREAD
    // -------------------------------------------------------
    is(STATE_SLAVEREAD) {
      prevClk := io.slave.scl
      when(~prevClk & io.slave.scl) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## io.slave.sdaIn
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_SLAVEREAD
        } .otherwise {
          when(shiftCounter === p.dataWidth.U) {
            sdata := i2cShift
            when(sctrla(7) === 1.U) {
              sstatus := sstatus | (1.U << 7.U)
            }
            stateReg := STATE_WAITSTOP
          } .otherwise {
            stateReg := STATE_SENDACKSLAVE
          }
        }
      }
    }
    
    // -------------------------------------------------------
    // MASTER SEND STOP
    // -------------------------------------------------------
    is(STATE_SENDSTOP) {
      // Master drives SDA high while SCL is high => STOP
      io.master.sdaOut := 1.U
      sclReg := true.B
      stateReg := STATE_IDLE
    }
    
    // -------------------------------------------------------
    // SLAVE WAIT STOP
    // -------------------------------------------------------
    is(STATE_WAITSTOP) {
      // If SCL=1 and SDA=1 => STOP => set APIF=1 (bit6), AP=0.
      when(io.slave.scl && io.slave.sdaIn === 1.U) {
        sstatus := (sstatus & ~1.U) | (1.U << 6)
        stateReg := STATE_IDLE
      } .otherwise {
        stateReg := STATE_WAITSTOP
      }
    }
  }

  // ------------------------------------------------------------------------
// 7) Helper Functions
// ------------------------------------------------------------------------
  def sendStartCondition(): Unit = {
    io.master.sdaOut := 0.U  // Drive SDA low for START condition
    ssFlag := 1.U            // Set start flag
    when(ssFlag === 1.U) {
      maddrFlag := false.B   // Clear address flag
      ssFlag    := 0.U       // Clear start flag
      sclReg    := false.B   // Drive SCL low
      addrShift := maddr     // Load address into shift register
      stateReg  := STATE_MASTERADDRESS  // Transition to MASTERADDRESS state
    }
  }

  def detectStartConditionSlave(): Bool = {
    val startDetected = WireInit(false.B)

    when(io.slave.sdaIn === 0.U) {
      ssFlag := 1.U
    }
    when(ssFlag === 1.U) {
      when(!io.slave.scl) {
        ssFlag := 0.U
        startDetected := true.B
      }
    }
    startDetected //Return
  }

  def detectStartConditionMaster(): Bool = {
    val startDetected = WireInit(false.B)

    when(io.master.sdaIn === 0.U) {
      ssFlag := 1.U
    }
    when(ssFlag === 1.U) {
      when(!io.master.scl) {
        ssFlag := 0.U
        startDetected := true.B
      }
    }
    startDetected //Return
  }

  def detectStopConditionSlave(): Bool = {
    val stopDetected = WireInit(false.B)
    prevClkBus := io.slave.scl
    prevSdaBus := io.slave.sdaIn

    when(~prevClkBus && io.slave.scl) {
      ssFlag := 1.U
    }
    when(ssFlag === 1.U) {
      when((io.slave.scl) && (prevSdaBus === 0.U) && (io.slave.sdaIn === 1.U)) {
        ssFlag := 0.U
        stopDetected := true.B
      }
    }
    stopDetected //Return
  }
//
  def detectStopConditionMaster(): Bool = {
    val stopDetected = WireInit(false.B)
    prevClkBus := io.master.scl
    prevSdaBus := io.master.sdaIn

    when(~prevClkBus && io.master.scl) {
      ssFlag := 1.U
    }
    when(ssFlag === 1.U) {
      when((io.master.scl) && (prevSdaBus === 0.U) && (io.master.sdaIn === 1.U)) {
        ssFlag := 0.U
        stopDetected := true.B
      }
    }
    stopDetected //Return
  }

}

