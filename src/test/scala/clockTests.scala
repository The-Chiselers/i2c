package tech.rocksavage.chiselware.I2C

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.apb.ApbTestUtils._ // For writeAPB
import org.scalatest.Assertions.fail

object clockTests {

  def masterClock(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock

    // Example target: 100 kHz SCL, 16 MHz system clock
    val sclFreq   = 100_000
    val clockFreq = 16_000_000

    // According to a simplified formula:
    // BAUD ≈ (PCLK / (2 * f_SCL)) - 1 
    // For 16MHz & 100kHz, that is (16e6 / (2*100e3)) - 1 = 79
    val expectedBaud = ((clockFreq / (2 * sclFreq)) - 1).toInt
    println(s"Calculated BAUD for 100 kHz: $expectedBaud")

    // Write the BAUD value
    val mbaudAddr = dut.registerMap.getAddressOfRegister("mbaud").get
    writeAPB(dut.io.apb, mbaudAddr.U, expectedBaud.U)

    // Also enable master mode by writing bit 0 of mctrla to 1
    val mctrlaAddr = dut.registerMap.getAddressOfRegister("mctrla").get
    // e.g., 0x01 => sets bit 0 = 1
    writeAPB(dut.io.apb, mctrlaAddr.U, 1.U)

    // Now step the simulation enough cycles to see SCL toggling
    var togglesSeen    = 0
    var lastSclState   = dut.io.sclOut.peek().litToBoolean

    // We expect SCL to have a period of about 160 system cycles at 100 kHz
    // (80 cycles low + 80 cycles high).
    // Step 300 cycles and see if we get ~2 toggles.
    for (_ <- 0 until 300) {
      dut.clock.step()
      val currentSclState = dut.io.sclOut.peek().litToBoolean
      if (currentSclState != lastSclState) {
        togglesSeen += 1
      }
      lastSclState = currentSclState
    }

    // We do a minimal check: at 100 kHz, in 300 cycles, we should see at least 2 toggles.
    if (togglesSeen < 2) {
      fail(s"Expected at least 2 SCL toggles over 300 cycles, but saw only $togglesSeen.")
    }
  }
}
