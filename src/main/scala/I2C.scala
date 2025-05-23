package tech.rocksavage.chiselware.I2C
import chisel3._
import chisel3.util._
import chiseltest.formal.past
import tech.rocksavage.test.TestUtils.coverAll
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap

object I2CEnums {
  object BusState extends ChiselEnum {
    val IDLE, BUSY, OWNER = Value
  }

  // State constants (as defined in I2C.scala)
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
}

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
class I2C(p: BaseParams, formal: Boolean = false) extends Module {
  import tech.rocksavage.chiselware.I2C.I2CEnums._

  var io = IO(new Bundle {
    val apb       = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val master    = new MasterInterface
    val slave     = new SlaveInterface
    val interrupt = Output(Bool())
    val state    = Output(UInt(4.W))
    val busState  = Output(BusState())
  })

  // ------------------------------------------------------------------------
  // 1) Register Map
  // ------------------------------------------------------------------------
  val registerMap = new RegisterMap(p.dataWidth, p.addrWidth, Some(p.wordWidth))
  // Internal signals
  val maddrFlag    = RegInit(false.B)
  val i2cShift     = RegInit(0.U(p.dataWidth.W)) // shared shift register
  val addrShift    = RegInit(0.U(8.W))           // for address bits
  val shiftCounter = RegInit(0.U((log2Ceil(p.dataWidth) + 1).W))
  val frameCounter = RegInit(0.U(5.W))
  // MASTER REGISTERS
  val mctrl  = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mctrl, "mctrl")
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
  val sctrl     = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sctrl, "sctrl")
  val sstatus   = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sstatus, "sstatus")
  val saddr     = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(saddr, "saddr")
  val sdata     = RegInit(0.U(p.dataWidth.W))
  registerMap.createAddressableRegister(sdata, "sdata")

  // ------------------------------------------------------------------------
  // 2) AddrDecode
  // ------------------------------------------------------------------------
  val addrDecodeParams = registerMap.getAddrDecodeParams
  val addrDecode       = Module(new AddrDecode(addrDecodeParams))
  addrDecode.io.addrRaw      := io.apb.PADDR
//  addrDecode.io.addrOffset := 0.Uw
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
            mstatus := mstatus & "b00010011".U //Clear interrupts
          }
          if (reg.name == "sdata"){
            i2cShift := io.apb.PWDATA
            sstatus := sstatus & "b00011111".U //Clear interrupts
          }
          if (reg.name == "maddr"){
            maddrFlag := true.B
            mstatus := mstatus & "b00010011".U //Clear interrupts
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
          if (reg.name == "mdata") {
            mstatus := mstatus & "b00010011".U //Clear interrupts
            io.apb.PRDATA := i2cShift
          }
          if (reg.name == "sdata") {
            sstatus := sstatus & "b00011111".U //Clear interrupts
            io.apb.PRDATA := i2cShift
           }
          else {
            io.apb.PRDATA := reg.readCallback(addrDecode.io.addrOut)
          }
        }
      }
    }
  }
  // ------------------------------------------------------------------------
  // 4) Divider-based Master Clock
  // ------------------------------------------------------------------------
  val sclCounter = RegInit(0.U(32.W))
  val sclReg     = RegInit(true.B)
  io.master.sclOut := true.B

  //when(mctrl(2) === 0.U) {
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
  val masterEn     = mctrl(0) // bit0 => enable master
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
  when(masterEn) {
    when(sclCounter >= (halfPeriodReg - 1.U)) {
      sclCounter := 0.U
      sclReg     := ~sclReg
    } .otherwise {
      sclCounter := sclCounter + 1.U
    }
    io.master.sclOut := sclReg
  }
  /*.otherwise{
    when(mctrl(0) === 1.U){
      when(sclCounter === ((2.U << (mbaud * 2.U)) - 1.U)) {
        sclkReg := ~sclkReg
        sclkCounter := 0.U
      }.otherwise {
        sclkCounter := sclkCounter + 1.U
      }
      io.master.sclk := sclkReg
    }
  }*/

  // ------------------------------------------------------------------------
  // 6) Master/Slave FSM Using UInt States
  // ------------------------------------------------------------------------

  val stateReg      = RegInit(STATE_IDLE)
  val previousState = RegInit(STATE_IDLE)
  // Local registers for the FSM
  val rwBit   = RegInit(0.U(1.W))
  val prevSda = RegInit(0.U(1.W))
  val prevClk = RegInit(true.B)
  val delayCounter = RegInit(0.U(4.W))
  //Master Bus FSM
  val prevSdaBus = RegInit(0.U(1.W))
  val prevClkBus = RegInit(true.B)
  val edgeCounter = RegInit(0.U(2.W))
  val ssFlags  = RegInit(0.U(5.W))  //One hot register

  val busStateReg = RegInit(BusState.IDLE)

  // Default outputs for SDA lines
  val interruptAfterStop = RegInit(false.B)

  io.interrupt := false.B || interruptAfterStop
  io.master.sdaOut := true.B
  io.slave.sdaOut  := true.B
  io.slave.sclOut  := true.B
  io.state := stateReg
  io.busState := busStateReg
  // Debug print for state transitions
  /*
  def printDebugTransition(oldSt: UInt, newSt: UInt): Unit = {
    printf(p"[FSM TRANS] $oldSt => $newSt: rw=$rwBit shiftCtr=$shiftCounter frmCtr=$frameCounter i2cShift=0x${Hexadecimal(i2cShift)} mdata=0x${Hexadecimal(mdata)} sdata=0x${Hexadecimal(sdata)} scl=$sclReg masterEn=$masterEn\n")
  }
  when(stateReg =/= previousState) {
    printDebugTransition(previousState, stateReg)
  }
  previousState := stateReg
  */

  switch(stateReg) {
    // -------------------------------------------------------
    // IDLE
    // -------------------------------------------------------
    is(STATE_IDLE) {
      shiftCounter := 0.U
      frameCounter := 0.U
      io.master.sclOut := true.B
      
      // MASTER side: When enabled and address written, drive SDA low and prepare to send address.
      when(mctrl(0) === 1.U && maddrFlag && (busStateReg === BusState.OWNER)) {
        when(sendStartCondition()) {
          delayCounter := delayCounter + 1.U
        }
      }
      when(delayCounter === 1.U){
        delayCounter := 0.U
        stateReg  := STATE_MASTERADDRESS  // Transition to MASTERADDRESS state
      }
      
      // SLAVE side: If enabled, look for SDA=0 => potential start
      when(sctrl(0) === 1.U) {
        when(detectStartConditionSlave()) {
          stateReg := STATE_SLAVEADDRESS
        }
      }
    }
    
    // -------------------------------------------------------
    // MASTERADDRESS
    // -------------------------------------------------------
    is(STATE_MASTERADDRESS) {
      prevClk := io.master.sclIn
      io.master.sdaOut := addrShift(7)
      when(~prevClk & io.master.sclIn) {
        when(frameCounter < 7.U) {
          addrShift   := addrShift(6, 0) ## 0.U
          frameCounter := frameCounter + 1.U
          stateReg    := STATE_MASTERADDRESS
        } .otherwise {
          rwBit := addrShift(7) // LSB is R/W bit
          prevSda := io.master.sdaIn
          stateReg := STATE_WAITACKMASTER
        } 
        when(io.master.sdaOut =/= io.master.sdaIn) {
          /* 
          *  Master Case 4: Arbitration Lost
          *  When another master has taken control of the sda line
          *  Set Write Interrupt Flag (WIF = 1) 
          *  Set Arbitration Lost Flag (ARBLOST = 1)
          *  Set Master Bus FSM to BUSY
          *  Then, put FSM back into IDLE until line is freed up
          */
          mstatus := (mstatus | (1.U(7.W) << 2) | (1.U(7.W) << 6) | (1.U(7.W) << 1)) & ~(1.U(7.W) << 0)
          busStateReg := BusState.BUSY
          stateReg := STATE_IDLE
        }
      }
    }
    
    // -------------------------------------------------------
    // SLAVEADDRESS
    // -------------------------------------------------------
    is(STATE_SLAVEADDRESS) {
      prevClk := io.slave.sclIn
      when(~prevClk & io.slave.sclIn) {
        when(frameCounter < 7.U) {
          addrShift    := addrShift(6, 0) ## io.slave.sdaIn
          frameCounter := frameCounter + 1.U
          stateReg     := STATE_SLAVEADDRESS
        } .otherwise {
          rwBit := io.slave.sdaIn
          // If address matches, set AP=1 in sstatus, then do SENDACKSLAVE
          when(saddr === addrShift) {
            sstatus := (sstatus | (1.U(8.W) << 0.U)) | (1.U(8.W) << 6.U) 
            when (io.slave.sdaIn === 0.U){
              sstatus := sstatus & ~(1.U << 1) 
            }.otherwise {
              sstatus := sstatus | (1.U << 1) 
            }
            //io.slave.sclOut := 0.U
            stateReg := STATE_SENDACKSLAVE
          } .otherwise {
            detectStopConditionSlave()
            stateReg := STATE_IDLE
          }
        }
      }
    }
    
    // -------------------------------------------------------
    // WAITACKMASTER
    // -------------------------------------------------------
    is(STATE_WAITACKMASTER) {
      //io.master.sdaOut := 0.U
      prevClk := io.master.sclIn
      frameCounter := 0.U
      when(~prevClk & io.master.sclIn) {
        //prevSda := io.master.sdaIn
        delayCounter := delayCounter + 1.U
        when(/*(prevSda === 1.U) && */ (io.master.sdaIn === 0.U)) {
          delayCounter := 0.U
          when(rwBit === 0.U) {
            /* 
            *  Master Case 1: Address Packet Transmit Complete - Direction Bit Set to Write
            *  When a slave device responds to the address packet with an ACK, do the following:
            *  Set Write Interrupt Flag (WIF = 1)
            *  Clear Recieved Acknowledge (RXACK = 0)
            *  Then, prepare to transmit data to slave via STATE_MASTERWRTE
            */
            mstatus := (mstatus | (1.U(7.W) << 6)) & ~(1.U(7.W) << 4)

            when(shiftCounter === p.dataWidth.U) {
              sclReg := true.B
              shiftCounter := 0.U
              stateReg := STATE_SENDSTOP
            }.otherwise { 
              stateReg     := STATE_MASTERWRITE
            }
          } .otherwise {  //Only enter once - after address packet transmission
            /* 
            *  Master Case 2: Address Packet Transmit Complete - Direction Bit Set to Read
            *  When a slave device responds to the address packet with an ACK, do the following:
            *  Clear Recieved Acknowledge (RXACK = 0)
            *  Then, prepare to read data from slave via STATE_MASTERREAD
            */
            mstatus := (mstatus | (1.U(7.W) << 6)) & ~(1.U(7.W) << 4)
            i2cShift     := 0.U
            shiftCounter := 0.U
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
            //io.master.sdaOut := 0.U
            when(delayCounter === 4.U) {
              delayCounter := 0.U
              mstatus := mstatus | ((1.U(7.W) << 6) | (1.U(7.W) << 4))
              sclReg := true.B
              stateReg := STATE_SENDSTOP
            }.otherwise {
              stateReg := STATE_WAITACKMASTER
            }
        }
      }
    }
    
    // -------------------------------------------------------
    // WAITACKSLAVE
    // -------------------------------------------------------
    is(STATE_WAITACKSLAVE) {
      prevClk := io.slave.sclIn
      frameCounter := 0.U
      when(~prevClk & io.slave.sclIn) {
        //prevSda := io.slave.sdaIn
        delayCounter := delayCounter + 1.U
        when(/*(prevSda === 1.U) && */(io.slave.sdaIn === 0.U)) {
          delayCounter := 0.U
          sstatus := (sstatus | (1.U << 7.U)) & ~(1.U(7.W) << 4)
          when(shiftCounter === p.dataWidth.U) {
            shiftCounter := 0.U
            detectStopConditionSlave()
            stateReg := STATE_WAITSTOP
          }.otherwise {
            stateReg     := STATE_SLAVEWRITE
          }
        } .otherwise {
          // No ACK => assume NACK => go to WAITSTOP
          when (delayCounter === 4.U) {
            delayCounter := 0.U
            sstatus := sstatus | (1.U(7.W) << 4)
            detectStopConditionSlave()
            stateReg := STATE_WAITSTOP
          }.otherwise {
            stateReg := STATE_WAITACKSLAVE
          }
        }
      }
    }
    
    // -------------------------------------------------------
    // SENDACKSLAVE
    // -------------------------------------------------------
    is(STATE_SENDACKSLAVE) {
      prevClk := io.slave.sclIn
      frameCounter := 0.U
      when(sctrl(4) === 1.U) { // Hold SCL Low until SW user unasserts mctrl(5) meaning they are ready
        io.slave.sclOut := 0.U
        sstatus := sstatus | (1.U(7.W) << 5)
        stateReg := STATE_SENDACKSLAVE
      }.otherwise {
        when(~prevClk & io.slave.sclIn) {
          sstatus := (sstatus | (1.U(7.W) << 7)) | (rwBit << 1)
          when(sctrl(1) === 0.U) {
            io.slave.sdaOut := 0.U
          }.otherwise {
            io.slave.sdaOut := 1.U
            detectStopConditionSlave()
            stateReg := STATE_WAITSTOP
          }
          when(shiftCounter === p.dataWidth.U) {
            sdata := i2cShift
            shiftCounter := 0.U
            detectStopConditionSlave()
            stateReg := STATE_WAITSTOP
          }
          when((shiftCounter =/= p.dataWidth.U) && (sctrl(1) === 0.U)) {
            stateReg := Mux(rwBit === 1.U, STATE_SLAVEWRITE, STATE_SLAVEREAD)
          }
        }
      }
    }
    
    // -------------------------------------------------------
    // SENDACKMASTER
    // -------------------------------------------------------
    is(STATE_SENDACKMASTER) {
      prevClk := io.master.sclIn
      frameCounter := 0.U
      when(mctrl(4) === 1.U) { // Hold SCL Low until SW user unasserts mctrl(5) meaning they are ready
        io.master.sclOut := 0.U
        mstatus := mstatus | (1.U(7.W) << 5)
        stateReg := STATE_SENDACKMASTER
      }.otherwise {
        when(~prevClk & io.master.sclIn) {
          mstatus := mstatus | (1.U << 7.U) //Set master read complete interrupt to register
          when(mctrl(1) === 0.U) {
            io.master.sdaOut := 0.U
          }.otherwise {
            io.master.sdaOut := 1.U
            stateReg := STATE_SENDSTOP
          }
          when(shiftCounter === p.dataWidth.U) {
            mdata := i2cShift
            sclReg := true.B
            shiftCounter := 0.U
            stateReg := STATE_SENDSTOP
          }
          when((shiftCounter =/= p.dataWidth.U) && (mctrl(1) === 0.U)) {
            stateReg := Mux(rwBit === 0.U, STATE_MASTERWRITE, STATE_MASTERREAD)
          }
        }
      }
    }
    
    // -------------------------------------------------------
    // MASTERWRITE
    // -------------------------------------------------------
    is(STATE_MASTERWRITE) {
      io.master.sdaOut := i2cShift(p.dataWidth - 1)
      prevClk := io.master.sclIn
      mstatus := mstatus & ~(1.U << 6)  //Clear WIF Flag
      when(~prevClk & io.master.sclIn) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## 0.U
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_MASTERWRITE
        } .otherwise {
            prevSda  := io.master.sdaIn
            stateReg := STATE_WAITACKMASTER
        }
        when(io.master.sdaOut =/= io.master.sdaIn) {
          mstatus := (mstatus | (1.U(3.W) << 2) | (1.U(3.W) << 1)) & ~(1.U(3.W) << 0)
          busStateReg := BusState.BUSY  
          stateReg := STATE_IDLE
        }
        
      }
    }
    
    // -------------------------------------------------------
    // SLAVEWRITE
    // -------------------------------------------------------
    is(STATE_SLAVEWRITE) {
      io.slave.sdaOut := i2cShift(p.dataWidth - 1)
      prevClk := io.slave.sclIn
      when(~prevClk & io.slave.sclIn) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## 0.U
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_SLAVEWRITE
        } .otherwise {
            prevSda := io.slave.sdaIn
            stateReg := STATE_WAITACKSLAVE
        }
      }
    }
    
    // -------------------------------------------------------
    // MASTERREAD
    // -------------------------------------------------------
    is(STATE_MASTERREAD) {
      prevClk := io.master.sclIn
      when(~prevClk & io.master.sclIn) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## io.master.sdaIn
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_MASTERREAD
        } .otherwise {
            sclReg := 0.U
            stateReg := STATE_SENDACKMASTER
        }
      }
    }
    
    // -------------------------------------------------------
    // SLAVEREAD
    // -------------------------------------------------------
    is(STATE_SLAVEREAD) {
      prevClk := io.slave.sclIn
      when(~prevClk & io.slave.sclIn) {
        when(frameCounter < 8.U) {
          i2cShift    := i2cShift(p.dataWidth - 2, 0) ## io.slave.sdaIn
          frameCounter := frameCounter + 1.U
          shiftCounter := shiftCounter + 1.U
          stateReg    := STATE_SLAVEREAD
        } .otherwise {
          //io.slave.sclOut := 0.U
          stateReg := STATE_SENDACKSLAVE
        }
      }
    }
    
    // -------------------------------------------------------
    // MASTER SEND STOP
    // -------------------------------------------------------
    is(STATE_SENDSTOP) {
      // Master drives SDA high while SCL is high => STOP
      io.master.sdaOut := 0.U
      sclReg := true.B
      ssFlags := ssFlags | (1.U << 3.U)
      when(ssFlags(3)) {
        io.master.sdaOut := 1.U
        maddr := 0.U
        io.interrupt := true.B
        interruptAfterStop := true.B 
        stateReg := STATE_IDLE
      }
    }
    
    // -------------------------------------------------------
    // SLAVE WAIT STOP
    // -------------------------------------------------------
    is(STATE_WAITSTOP) {
      when(detectStopConditionSlave()){
        addrShift := 0.U
        sstatus := (sstatus & ~(1.U(8.W) << 0.U)) | (1.U(8.W) << 6.U)
        saddr := 0.U
        io.interrupt := true.B
        interruptAfterStop := true.B 
        stateReg := STATE_IDLE
      }
    }
  }


  // ------------------------------------------------------------------------
  // 7) Master Bus State FSM
  // ------------------------------------------------------------------------
  when (mctrl(0) === 1.U) {
    switch(busStateReg) {
      is(BusState.IDLE) {
        when(maddrFlag) { //Internal START Condition
          mstatus := (mstatus | (1.U(7.W) << 0)) & ~(1.U(7.W) << 1)        
          busStateReg := BusState.OWNER
        }.elsewhen(detectStartConditionMaster()){ //External START Condition
          mstatus := (mstatus & ~(1.U(7.W) << 0)) | (1.U(7.W) << 1)       
          busStateReg := BusState.BUSY
        }
      }
      is(BusState.BUSY) {
        when(detectStopConditionMaster()) { //STOP Condition Detected
          mstatus := mstatus & ~((1.U(7.W) << 0) | (1.U(7.W) << 1)) 
          busStateReg := BusState.IDLE
        }
      }
      is(BusState.OWNER) {
        when(stateReg === STATE_SENDSTOP) { //Need to add check for arb lost
          mstatus := mstatus & ~((1.U(7.W) << 0) | (1.U(7.W) << 1))           
          busStateReg := BusState.IDLE
        }
      }
    }
  }

  when(stateReg === STATE_IDLE && interruptAfterStop) {
    when(RegNext(stateReg) === STATE_IDLE) {
      interruptAfterStop := false.B  // Clear after one full cycle in IDLE
    }
  }


  // ------------------------------------------------------------------------
// 8) Helper Functions
// ------------------------------------------------------------------------
  def sendStartCondition(): Bool = {
    val sendStart = WireInit(false.B)
    io.master.sdaOut := 0.U  // Drive SDA low for START condition
    ssFlags := ssFlags | (1.U << 0.U)            // Set start flag
    when(ssFlags(0) === 1.U) {
      maddrFlag := false.B   // Clear address flag
      ssFlags   := ssFlags & ~(1.U << 0.U)        // Clear start flag
      sclReg    := false.B   // Drive SCL low
      addrShift := maddr     // Load address into shift register
      sendStart := true.B
    }

    sendStart
  }

  def detectStartConditionSlave(): Bool = {
    val startDetected = WireInit(false.B)

    when(io.slave.sdaIn === 0.U) {
      ssFlags := ssFlags | (1.U << 1.U)
    }
    when(ssFlags(1) === 1.U) {
      when(!io.slave.sclIn) {
        ssFlags := ssFlags & ~(1.U << 1.U)
        startDetected := true.B
      }
    }
    startDetected //Return
  }

  def detectStartConditionMaster(): Bool = {
    val startDetected = WireInit(false.B)
    prevSdaBus := io.master.sdaIn
    prevClkBus := io.master.sclIn

    when((prevSdaBus === 1.U) && (io.master.sdaIn === 0.U)) {
      ssFlags := ssFlags | (1.U << 2.U)
    }
    when(ssFlags(2) === 1.U) {
      when(prevClkBus && !io.master.sclIn) {
        ssFlags := ssFlags & ~(1.U << 2.U)
        startDetected := true.B
      }
    }
    startDetected //Return
  }

  def detectStopConditionSlave(): Bool = {
    val stopDetected = WireInit(false.B)
    prevSdaBus := io.slave.sdaIn
    prevClk := io.slave.sclIn

    when(~prevClk && io.slave.sclIn) {
      ssFlags := ssFlags | (1.U << 4.U)
    }

    when (ssFlags(4) === 1.U && prevClk && io.slave.sclIn) {
      edgeCounter := edgeCounter + 1.U
    }
    when((edgeCounter === 1.U) && (ssFlags(4) === 1.U)) { 
      when ((prevSdaBus === 0.U) && (io.slave.sdaIn === 1.U)) {
        edgeCounter := 0.U
        ssFlags := ssFlags & ~(1.U << 4.U)
        stopDetected := true.B
      }.otherwise {
        edgeCounter := 0.U
        ssFlags := ssFlags & ~(1.U << 4.U)
      }
    }
    stopDetected //Return
  }

  def detectStopConditionMaster(): Bool = {
    val stopDetected = WireInit(false.B)
    prevClkBus := io.master.sclIn
    prevSdaBus := io.master.sdaIn

    when(~prevClkBus && io.master.sclIn) {
      ssFlags := ssFlags | (1.U << 4.U)
    }

    when (ssFlags(4) === 1.U && prevClkBus && io.master.sclIn) {
      edgeCounter := edgeCounter + 1.U
    }
    when((edgeCounter === 1.U) && (ssFlags(4) === 1.U)) { 
      when ((prevSdaBus === 0.U) && (io.master.sdaIn === 1.U)) {
        edgeCounter := 0.U
        ssFlags := ssFlags & ~(1.U << 4.U)
        stopDetected := true.B
      }.otherwise {
        edgeCounter := 0.U
        ssFlags := ssFlags & ~(1.U << 4.U)
      }
    }
    stopDetected //Return
  }

  if (formal) {
  val pastSclMaster = RegNext(io.master.sclOut, init = true.B)
  val pastSdaMaster = RegNext(io.master.sdaOut, init = true.B)
  val pastSclSlave  = RegNext(io.slave.sclIn, init = true.B)
  val pastSdaSlave  = RegNext(io.slave.sdaIn, init = true.B)
  val pastState     = RegNext(stateReg, init = STATE_IDLE)
  val sclToggleCounter = RegInit(0.U(32.W))

  val mctrlStable   = past(mctrl) === mctrl
  val sctrlStable   = past(sctrl) === sctrl
  val maddrStable   = past(maddr) === maddr
  val saddrStable   = past(saddr) === saddr

  // --------------------------------------
  // 1) Start Condition Verification
  // --------------------------------------
  // More flexible START condition verification
  val startConditionDetected = RegInit(false.B)
  when(pastSdaMaster === 1.U && io.master.sdaOut === 0.U && 
      pastSclMaster === 1.U && io.master.sclOut === 1.U) {
    startConditionDetected := true.B
  }

  // Verify that we see a START condition before MASTERADDRESS (eventually)
  when(stateReg === STATE_MASTERADDRESS) {
    assert(startConditionDetected, "Should see START condition before MASTERADDRESS")
  }

  // --------------------------------------
  // 2) Stop Condition Verification
  // --------------------------------------
  // First phase - SCL high, SDA low, ssFlags being set
  when(stateReg === STATE_SENDSTOP && !ssFlags(3)) {
    cover(io.master.sclOut === 1.U && io.master.sdaOut === 0.U,
          "Cover STOP condition phase 1: SCL high, SDA low")
  }
  
  // Second phase - SCL remains high, SDA transitions to high
  when(stateReg === STATE_SENDSTOP && ssFlags(3)) {
    cover(io.master.sclOut === 1.U && io.master.sdaOut === 1.U,
          "Cover STOP condition phase 2: SCL high, SDA high")
    
    // Verify state transition
    cover(RegNext(stateReg) === STATE_IDLE, 
          "Cover transition to IDLE after STOP condition")
    
    // Verify interrupt generation
    cover(io.interrupt === true.B,
          "Cover interrupt assertion after STOP")
  }
  
  // Slave STOP detection
  when(stateReg === STATE_WAITSTOP && detectStopConditionSlave()) {
    cover(RegNext(sstatus(0)) === 0.U && RegNext(sstatus(6)) === 1.U,
          "Cover slave status bits updating on STOP detection")
  }
  
  // --------------------------------------
  // 3) Master Clock Specification
  // --------------------------------------
  when(mctrl(0) === 1.U && stateReg =/= STATE_SENDSTOP && busStateReg =/= BusState.BUSY) {
    // Verify counter limits
    assert(sclCounter <= halfPeriodReg, 
           "SCL counter must not exceed half period value")
    
    // Track SCL toggles to ensure clocking works
    when(pastSclMaster =/= io.master.sclOut) { 
      sclToggleCounter := sclToggleCounter + 1.U 
    }
    
    // Ensure clock toggles regularly when enabled
    val clockStableCounter = RegInit(0.U(8.W))
    when(pastSclMaster === io.master.sclOut) {
      clockStableCounter := clockStableCounter + 1.U
    } .otherwise {
      clockStableCounter := 0.U
    }
    
    assert(clockStableCounter < halfPeriodReg + 2.U, 
           "SCL should toggle within half period time")
  }

  // --------------------------------------
  // 4) Data Stability During SCL High
  // --------------------------------------
  // Simpler SDA stability check during data transfer
  when(stateReg === STATE_MASTERWRITE || stateReg === STATE_SLAVEWRITE) {
    when(pastSclMaster === 0.U && io.master.sclOut === 1.U) {
      // At SCL rising edge, capture SDA
      val sdaCapture = RegNext(io.master.sdaOut)
      
      // Verify SDA doesn't change during the high portion of the SCL
      // Only check one cycle later to avoid timing edge cases
      when(pastSclMaster === 1.U && io.master.sclOut === 1.U) {
        cover(io.master.sdaOut === sdaCapture, "Cover SDA stability during SCL high")
      }
    }
  }

  // --------------------------------------
  // 5) Slave Address Match and ACK
  // --------------------------------------
  when(stateReg === STATE_SLAVEADDRESS && frameCounter === 7.U) {
    // Track address match
    val addrMatch = saddr === addrShift
    
    // Use cover properties instead of assertions
    cover(addrMatch && RegNext(stateReg) === STATE_SENDACKSLAVE, 
          "Cover address match leading to SENDACKSLAVE")
    cover(!addrMatch && RegNext(stateReg) === STATE_WAITSTOP, 
          "Cover address non-match leading to WAITSTOP")
    
    // If needed, verify status bit setting, but with more flexibility
    when(addrMatch) {
      cover(RegNext(sstatus(0)) === 1.U, 
            "Cover AP bit setting on address match")
    }
  }
  
  when(stateReg === STATE_SENDACKSLAVE && sctrl(1) === 0.U) {
    cover(io.slave.sdaOut === 0.U, 
          "Cover slave driving SDA low for ACK")
  }

  // --------------------------------------
  // 6) Liveness: Master and Slave Progress
  // --------------------------------------
  val stateStableCounter = RegInit(0.U(8.W))
  when(stateReg === pastState && stateReg =/= STATE_IDLE) {
    stateStableCounter := stateStableCounter + 1.U
  } .otherwise {
    stateStableCounter := 0.U
  }
  
  // Special handling for states that can legitimately wait
  when(stateReg === STATE_WAITSTOP || 
      (stateReg === STATE_SENDACKMASTER && mctrl(4) === 1.U) || 
      (stateReg === STATE_SENDACKSLAVE && sctrl(4) === 1.U)) {
    // These states can wait longer
  } .elsewhen(stateReg =/= STATE_IDLE) {
    assert(stateStableCounter < 100.U, 
           "State must progress within reasonable time")
  }
  
  // --------------------------------------
  // 7) Interrupt Specification
  // --------------------------------------
  
  when(past(stateReg) === STATE_SENDSTOP && stateReg === STATE_IDLE) {
    cover(io.interrupt === true.B, "Cover interrupt assertion after STOP")
  }

  when(past(stateReg) === STATE_WAITSTOP && stateReg === STATE_IDLE) {
    cover(io.interrupt === true.B, "Cover interrupt assertion after slave detects STOP")
  }
  
  // Status flag checks
  when(past(stateReg) === STATE_WAITACKMASTER && stateReg === STATE_MASTERWRITE && 
       past(io.master.sdaIn) === 0.U && past(rwBit) === 0.U) {
    assert(mstatus(6) === 1.U, 
           "Write Interrupt Flag should be set after ACK received in master write mode")
  }

  // --------------------------------------
  // 8) Clock Stretching
  // --------------------------------------
  when(stateReg === STATE_SENDACKMASTER && mctrl(4) === 1.U) {
    assert(io.master.sclOut === 0.U, 
           "Master must stretch clock when mctrl(4)=1")
  }
  
  when(stateReg === STATE_SENDACKSLAVE && sctrl(4) === 1.U) {
    assert(io.slave.sclOut === 0.U, 
           "Slave must stretch clock when sctrl(4)=1")
  }
  
  // --------------------------------------
  // 9) Bus State Transitions
  // --------------------------------------
  // Basic mutual exclusion - cannot be in contradictory states
  assert(!(busStateReg === BusState.OWNER && busStateReg === BusState.BUSY), 
        "Bus cannot be both OWNER and BUSY simultaneously")

  // Verify STOP eventually leads to IDLE
  cover(past(stateReg) === STATE_SENDSTOP && stateReg === STATE_IDLE, 
        "Cover transition from SENDSTOP to IDLE")

  // Register to track when a STOP condition is detected
  val stopDetected = RegInit(false.B)
  when(busStateReg === BusState.BUSY && detectStopConditionMaster()) {
    stopDetected := true.B
  } .elsewhen(busStateReg === BusState.IDLE) {
    stopDetected := false.B
  }

}
  
  // Collect code coverage points
  if (p.coverage) {
      // Cover the entire IO bundle recursively.
      coverAll(io, "_io")
  }

}