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

  //Internal Flags
  val maddrFlag = RegInit(false.B)
  
  // Shift Registers
  val i2cShift = RegInit(0.U(p.dataWidth.W))
  val addrShift = RegInit(0.U(8.W))
  val shiftCounter = RegInit(0.U((log2Ceil(p.dataWidth) + 1).W))
  val frameCounter = RegInit(0.U((5.W)))

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
          when((reg.name == "mdata").B || (reg.name == "sdata").B){
            i2cShift := io.apb.PWDATA
          }.otherwise{
            reg.writeCallback(addrDecode.io.addrOffset, io.apb.PWDATA)
          }
          when((reg.name == "maddr").B){
            maddrFlag := true.B
          }
        }
      }
    } .otherwise {
      for (reg <- registerMap.getRegisters) {
        when(addrDecode.io.sel(reg.id)) {
          when((reg.name == "mdata").B || (reg.name == "sdata").B){
            io.apb.PRDATA := i2cShift
          }.otherwise {
            io.apb.PRDATA := reg.readCallback(addrDecode.io.addrOffset)
          }
        }
      }
    }
  }

  // ------------------------------------------------------------------------
  // State Machine Initilization
  // ------------------------------------------------------------------------  
  object State extends ChiselEnum {
    val idle, 
    masterAddress, slaveAddress, 
    waitAckMaster, waitAckSlave, 
    sendAckMaster, sendAckSlave, 
    masterWrite, slaveWrite, 
    masterRead, slaveRead, 
    sendStop, waitStop = Value
  }
  import State._
  val stateReg = RegInit(idle)

  // ------------------------------------------------------------------------
  // Divider Module Instantiation
  // ------------------------------------------------------------------------  
  /*
    val dividerFreq = Module(new Divider())
    dividerFreq.io.numerator := (p.clkFreq.U * 1000000.U) 
    dividerFreq.io.denominator := (10.U + 2.U * mbaud)
    dividerFreq.io.start := 0.U

    val dividerPer = Module(new Divider())
    dividerPer.io.numerator := 0.U
    dividerPer.io.denominator := 0.U
    dividerPer.io.start := 0.U
  */
  // ------------------------------------------------------------------------
  // Baud Rate Generator (Master)
  // ------------------------------------------------------------------------
    val sclCounter    = RegInit(0.U(16.W))
    val sclReg        = RegInit(true.B)
    val prevClk       = RegInit(true.B)
    val sclPer        = RegInit(0.U(32.W))
    io.master.scl := true.B

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
    }
    /*
    when(mctrla(0) === 1.U){
      dividerFreq.io.start := 1.U
      when(dividerFreq.io.valid){
        dividerPer.io.numerator := (p.clkFreq.U * 1000000.U) 
        dividerPer.io.denominator := dividerFreq.io.result
        dividerPer.io.start := 1.U
      }
      when(dividerPer.io.valid){
        sclPer := dividerPer.io.result
      }

      when(sclCounter === (sclPer - 1.U)) {
          sclCounter := 0.U
          sclReg := ~sclReg
      } .otherwise {
          sclCounter := sclCounter + 1.U
      }
      io.master.scl := sclReg
      
    }.otherwise {
      io.master.scl := false.B
      io.master.sdaOut := 0.U
    }
    */
  // ------------------------------------------------------------------------
  // State Machine Operation
  // ------------------------------------------------------------------------  

    //Internal Regs
    val rwBit = RegInit(0.U(1.W))
    val ssFlag = RegInit(0.U(1.W))
    val prevSda = RegInit(0.U(1.W))

    io.interrupt := 0.U //Temp
    io.slave.sdaOut := 1.U //Temp
    io.master.sdaOut := 1.U //Default Case

    switch(stateReg) {
      is(idle) {
        shiftCounter := 0.U
        frameCounter := 0.U
        when(mctrla(0) === 1.U) { // Master Mode
            sclReg := true.B    //Drive SCL High in IDLE mode
            when(maddrFlag){  //When Master Address is Written
                io.master.sdaOut := 0.U //Start Condition - Drive SDA Low before SCL switches hi to low
                ssFlag := 1.U
                stateReg := idle
                when(ssFlag === 1.U) {
                  maddrFlag := false.B
                  ssFlag := 0.U
                  sclReg := false.B   //Drive SCL Low after SDA
                  addrShift := maddr  //Load address to shift out to slave
                  stateReg := masterAddress //Go to address shift state
                }
            }
        }
        when(sctrla(0) === 1.U){
          when((io.slave.sdaIn === 0.U)) {
            ssFlag := 1.U  
            stateReg := idle           
          }
          when(ssFlag === 1.U){
            when(!io.slave.scl){
              ssFlag := 0.U
              stateReg := slaveAddress
            }
          } 
        }
      }
      is(masterAddress){
        io.master.sdaOut := addrShift(p.dataWidth - 1)
        prevClk := sclReg
        when(~prevClk & io.master.scl){
          when(frameCounter < 7.U) {
            addrShift := addrShift(p.dataWidth - 2, 0) ## 0.U
            frameCounter := frameCounter + 1.U
            stateReg := masterAddress
          }.otherwise {
              rwBit := addrShift(p.dataWidth - 1)
              stateReg := waitAckMaster
          }
        }
      }
      is(slaveAddress){
        prevClk := io.slave.scl
        when(~prevClk & io.slave.scl) {  
          when(frameCounter < 7.U) {          
            addrShift := addrShift(p.dataWidth - 2, 0) ## io.slave.sdaIn
            frameCounter := frameCounter + 1.U
            stateReg := slaveAddress
          }.otherwise{
            rwBit := io.slave.sdaIn
            stateReg := Mux(saddr === addrShift, sendAckSlave, idle)
          }
        }
      }
      is(waitAckMaster){   //How do we know which slave sent the ACK?
          prevClk := sclReg   //What happens if we never get ACK?
          prevSda := io.master.sdaIn
          frameCounter := 0.U
          when(~prevClk & io.master.scl){
              when((prevSda === 1.U) & (io.master.sdaIn === 0.U)) {     //How do we know slave only sent 1 bit ack?
                stateReg := Mux(rwBit === 1.U, masterWrite, masterRead)
              }
          }
      }
      is(waitAckSlave){
        prevClk := io.slave.scl
        prevSda := io.slave.sdaIn
        frameCounter := 0.U
        when(~prevClk & io.slave.scl){
          when((prevSda === 1.U) & (io.slave.sdaIn === 0.U)) {     //How do we know slave only sent 1 bit ack?
            stateReg := Mux(rwBit === 1.U, slaveRead, slaveWrite)
          }
        }
      }
      is(sendAckSlave){
        prevClk := io.slave.scl
        io.slave.sdaOut := 0.U
        frameCounter := 0.U
        when(~prevClk & io.slave.scl){
          stateReg := Mux(rwBit === 1.U, slaveRead, slaveWrite)
        }
      }
      is(sendAckMaster){
        prevClk := sclReg
        io.master.sdaOut := 0.U
        frameCounter := 0.U
        when(~prevClk & io.master.scl){
          stateReg := Mux(rwBit === 1.U, masterWrite, masterRead)
        }
      }
      is(masterWrite){
        io.master.sdaOut := i2cShift(p.dataWidth - 1)
        prevClk := sclReg
        when(~prevClk & io.master.scl) {
          when(frameCounter < 8.U){
            i2cShift := i2cShift(p.dataWidth - 2, 0) ## 0.U
            frameCounter := frameCounter + 1.U
            shiftCounter := shiftCounter + 1.U
            stateReg := masterWrite
          }.otherwise{
            when(shiftCounter === p.dataWidth.U){ //When should we send stop condition?
              sclReg := true.B
              stateReg := sendStop
            }.otherwise {
              stateReg := waitAckMaster
            }
          }
        }
      }
      is(slaveWrite) {
        io.slave.sdaOut := i2cShift(p.dataWidth - 1)
        prevClk := io.slave.scl
        when(~prevClk & io.slave.scl) {
          when(frameCounter < 8.U){
            i2cShift := i2cShift(p.dataWidth - 2, 0) ## 0.U
            frameCounter := frameCounter + 1.U
            shiftCounter := shiftCounter + 1.U
            stateReg := slaveWrite
          }.otherwise{
            when(shiftCounter === p.dataWidth.U){
              stateReg := waitStop
            }.otherwise {
              stateReg := waitAckSlave
            }
          }
        }     
      }
      is(masterRead){
        prevClk := sclReg
        when(~prevClk & io.master.scl) {
          when(frameCounter < 8.U){
            i2cShift := i2cShift(p.dataWidth - 2, 0) ## io.master.sdaIn
            frameCounter := frameCounter + 1.U
            shiftCounter := shiftCounter + 1.U
            stateReg := masterRead
          }.otherwise{
            when(shiftCounter === p.dataWidth.U){
              sclReg := true.B
              stateReg := sendStop
            }.otherwise {
              stateReg := sendAckMaster
            }
          }
        }          
      }
      is(slaveRead){
        prevClk := io.slave.scl
        when(~prevClk & io.slave.scl) {
          when(frameCounter < 8.U){
            i2cShift := i2cShift(p.dataWidth - 2, 0) ## io.slave.sdaIn
            frameCounter := frameCounter + 1.U
            shiftCounter := shiftCounter + 1.U
            stateReg := slaveRead
          }.otherwise{
            when(shiftCounter === p.dataWidth.U){
              stateReg := waitStop
            }.otherwise {
              stateReg := sendAckSlave
            }
          }
        }          
      }
      is(sendStop){
          io.master.sdaOut := 1.U
          sclReg := true.B
          stateReg := idle
      }
      is(waitStop){
        when(io.slave.scl) {
          ssFlag := 1.U     
          stateReg := waitStop       
        }
        when(ssFlag === 1.U){
          when(io.slave.sdaIn === 1.U){
            ssFlag := 0.U
            stateReg := idle
          }
        }
      }
    }
}