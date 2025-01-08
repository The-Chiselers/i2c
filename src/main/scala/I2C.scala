// (c) 2024 Rocksavage Technology, Inc.
// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)
package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap

class I2C(val regWidth: Int = 8, val dataWidth: Int = 8, val addressWidth: Int = 16) extends Module {
  val io = IO(new Bundle {
    val apb = new ApbBundle(ApbParams(dataWidth, addressWidth))
    val sclIn = Input(Bool())        // I2C clock input
    val sdaIn = Input(Bool())        // I2C data input
    val sclOut = Output(Bool())      // I2C clock output
    val sdaOut = Output(Bool())      // I2C data output
    val interrupt = Output(Bool())   // Interrupt signal
  })

  // Create a RegisterMap to manage the addressable registers
  val registerMap = new RegisterMap(regWidth, addressWidth)

  // ########################
  // I2C MASTER REGISTERS
  // ########################
  val mctrla: UInt = RegInit(0.U(regWidth.W))   // Master Control A Register
  registerMap.createAddressableRegister(mctrla, "MCTRLA")

  val mctrlb: UInt = RegInit(0.U(regWidth.W))   // Master Control B Register
  registerMap.createAddressableRegister(mctrlb, "MCTRLB")

  val mstatus: UInt = RegInit(0.U(regWidth.W))  // Master Status Register
  registerMap.createAddressableRegister(mstatus, "MSTATUS")

  val mbaud: UInt = RegInit(0.U(regWidth.W))    // Master Baud Rate Register
  registerMap.createAddressableRegister(mbaud, "MBAUD")

  val maddr: UInt = RegInit(0.U(regWidth.W))    // Master Address Register
  registerMap.createAddressableRegister(maddr, "MADDR")

  val mdata: UInt = RegInit(0.U(dataWidth.W))   // Master Data Register
  registerMap.createAddressableRegister(mdata, "MDATA")

  // ########################
  // I2C SLAVE REGISTERS
  // ########################
  val sctrla: UInt = RegInit(0.U(regWidth.W))   // Slave Control A Register
  registerMap.createAddressableRegister(sctrla, "SCTRLA")

  val sctrlb: UInt = RegInit(0.U(regWidth.W))   // Slave Control B Register
  registerMap.createAddressableRegister(sctrlb, "SCTRLB")

  val sstatus: UInt = RegInit(0.U(regWidth.W))  // Slave Status Register
  registerMap.createAddressableRegister(sstatus, "SSTATUS")

  val saddr: UInt = RegInit(0.U(regWidth.W))    // Slave Address Register
  registerMap.createAddressableRegister(saddr, "SADDR")

  val sdata: UInt = RegInit(0.U(dataWidth.W))   // Slave Data Register
  registerMap.createAddressableRegister(sdata, "SDATA")

  val saddrmask: UInt = RegInit(0.U(regWidth.W)) // Slave Address Mask Register
  registerMap.createAddressableRegister(saddrmask, "SADDRMASK")

  // ########################
  // AddrDecode Integration
  // ########################
  val addrDecodeParams = registerMap.getAddrDecodeParams
  val addrDecode = Module(new AddrDecode(addrDecodeParams))
  addrDecode.io.addr := io.apb.PADDR
  addrDecode.io.addrOffset := 0.U
  addrDecode.io.en := true.B
  addrDecode.io.selInput := true.B

  // ########################
  // APB Interface Signals
  // ########################
  io.apb.PREADY := (io.apb.PENABLE && io.apb.PSEL)
  io.apb.PSLVERR := addrDecode.io.errorCode === AddrDecodeError.AddressOutOfRange
  io.apb.PRDATA := 0.U

  // Register Read/Write Logic
  when(io.apb.PSEL && io.apb.PENABLE) {
    when(io.apb.PWRITE) {
      for (reg <- registerMap.getRegisters) {
        when(addrDecode.io.sel(reg.id)) {
          reg.writeCallback(addrDecode.io.addrOffset, io.apb.PWDATA)
        }
      }
    }.otherwise {
      for (reg <- registerMap.getRegisters) {
        when(addrDecode.io.sel(reg.id)) {
          io.apb.PRDATA := reg.readCallback(addrDecode.io.addrOffset)
        }
      }
    }
  }

  // ########################
  // Slave Operation FSM
  // ########################
  val slaveFSM = RegInit(0.U(3.W)) // Slave FSM States
  val sdaSlaveReg = RegInit(true.B) // SDA line (output when driving)
  val sclSlaveReg = RegInit(true.B) // SCL line (output when driving)

  val slaveAddressMatch = Wire(Bool()) // Indicates address match
  val receivedData = RegInit(0.U(dataWidth.W)) // Register to store received data
  val dataToSend = Wire(UInt(dataWidth.W)) // Data to send during transmission

  slaveAddressMatch := (io.sdaIn === (saddr & ~saddrmask)) // Compare input address with configured address

  val idle :: addressMatch :: dataReceive :: dataTransmit :: stopCondition :: Nil = Enum(5)
  switch(slaveFSM) {
    is(idle) {
      when(io.sdaIn === false.B && io.sclIn === true.B) {
        slaveFSM := addressMatch
      }
    }

    is(addressMatch) {
      when(slaveAddressMatch) {
        sstatus := 0x01.U
        slaveFSM := dataReceive
      }.otherwise {
        slaveFSM := idle
      }
    }

    is(dataReceive) {
      when(io.sclIn === true.B) {
        receivedData := Cat(receivedData(dataWidth - 2, 0), io.sdaIn)
        sstatus := 0x02.U
      }
      when(io.sdaIn === true.B && io.sclIn === true.B) {
        slaveFSM := stopCondition
      }
    }

    is(dataTransmit) {
      when(io.sclIn === false.B) {
        sdaSlaveReg := dataToSend(dataWidth - 1)
        dataToSend := Cat(dataToSend(dataWidth - 2, 0), 0.U)
      }
      when(io.sdaIn === true.B && io.sclIn === true.B) {
        slaveFSM := stopCondition
      }
    }

    is(stopCondition) {
      sdaSlaveReg := true.B
      sclSlaveReg := true.B
      slaveFSM := idle
    }
  }

  // ########################
  // Master Operation FSM
  // ########################
  val masterFSM = RegInit(0.U(3.W)) // Master FSM States
  val sclMasterReg = RegInit(true.B) // SCL line (output)
  val sdaMasterReg = RegInit(true.B) // SDA line (output)

  val bitCounter = RegInit(0.U(4.W)) // Counter for bit transmission
  val transmittedData = RegInit(0.U(dataWidth.W)) // Data to be transmitted
  val receivedMasterData = RegInit(0.U(dataWidth.W)) // Register to store received data

  val idleMaster :: startCondition :: sendAddress :: dataTransmitMaster :: dataReceiveMaster :: stopConditionMaster :: Nil = Enum(6)
  switch(masterFSM) {
    is(idleMaster) {
      when(mctrla(0)) {
        masterFSM := startCondition
      }
    }

    is(startCondition) {
      sdaMasterReg := false.B
      sclMasterReg := true.B
      when(sdaMasterReg === false.B && sclMasterReg === true.B) {
        masterFSM := sendAddress
        bitCounter := 0.U
      }
    }

    is(sendAddress) {
      val slaveAddress = maddr(7, 1)
      val rwBit = maddr(0)
      val addressWithRW = Cat(slaveAddress, rwBit)

      when(bitCounter < 8.U) {
        sdaMasterReg := addressWithRW(7 - bitCounter)
        sclMasterReg := !sclMasterReg
        when(sclMasterReg === true.B) {
          bitCounter := bitCounter + 1.U
        }
      }.otherwise {
        masterFSM := dataTransmitMaster
      }
    }

    is(dataTransmitMaster) {
      when(bitCounter < 8.U) {
        sdaMasterReg := transmittedData(7 - bitCounter)
        sclMasterReg := !sclMasterReg
        when(sclMasterReg === true.B) {
          bitCounter := bitCounter + 1.U
        }
      }.otherwise {
        masterFSM := stopConditionMaster
      }
    }

    is(dataReceiveMaster) {
      when(bitCounter < 8.U) {
        when(sclMasterReg === true.B) {
          receivedMasterData := Cat(receivedMasterData(dataWidth - 2, 0), io.sdaIn)
          bitCounter := bitCounter + 1.U
        }
        sclMasterReg := !sclMasterReg
      }.otherwise {
        masterFSM := stopConditionMaster
      }
    }

    is(stopConditionMaster) {
      sdaMasterReg := true.B
      sclMasterReg := true.B
      when(sdaMasterReg === true.B && sclMasterReg === true.B) {
        masterFSM := idleMaster
      }
    }
  }

  // Drive Outputs
  io.sclOut := sclSlaveReg & sclMasterReg
  io.sdaOut := sdaSlaveReg & sdaMasterReg
  io.interrupt := (slaveFSM === addressMatch) || (masterFSM === stopConditionMaster)
}
