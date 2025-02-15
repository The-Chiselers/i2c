package tech.rocksavage.chiselware.I2C

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.apb.ApbTestUtils._ // For writeAPB
import org.scalatest.Assertions.fail
import scala.util.Random

object clockTests {

  def masterClock(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock
    dut.clock.setTimeout(0) 

    // Example target: 500 kHz SCL
    val sclFreq   = 500_000
    val clockFreq = 50_000_000
    val expectedBaud = ((clockFreq / (2 * sclFreq)) - 5).toInt
    println(s"Calculated BAUD for 500 kHz: $expectedBaud")

    // 1) Write the BAUD value
    val mbaudAddr = dut.registerMap.getAddressOfRegister("mbaud").get
    writeAPB(dut.io.apb, mbaudAddr.U, expectedBaud.U)

    // 2) Enable master mode
    val mctrlaAddr = dut.registerMap.getAddressOfRegister("mctrla").get
    writeAPB(dut.io.apb, mctrlaAddr.U, 1.U)

    // 3) Wait ~20 cycles for the divider FSM to compute halfPeriod, etc.
    dut.clock.step(100)

    // 4) Now step the simulation enough cycles to see toggles
    var togglesSeen  = 0
    var lastSclState = dut.io.master.scl.peek().litToBoolean

    for (_ <- 0 until 300) {
      dut.clock.step()
      val currentScl = dut.io.master.scl.peek().litToBoolean
      if (currentScl != lastSclState) {
        togglesSeen += 1
      }
      lastSclState = currentScl
    }

    if (togglesSeen < 2) {
      fail(s"Expected at least 2 SCL toggles over 300 cycles, but saw only $togglesSeen.")
    }
  }

  def dividerBasicCheck(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock
    dut.clock.setTimeout(0)

    val targetSclHz  = 400_000
    val clockFreqHz  = params.clkFreq * 1_000_000
    val computedBaud = (clockFreqHz / targetSclHz) - 5
    val mbaudAddr    = dut.registerMap.getAddressOfRegister("mbaud").get
    val mctrlaAddr   = dut.registerMap.getAddressOfRegister("mctrla").get

    // 1) Write the BAUD
    writeAPB(dut.io.apb, mbaudAddr.U, computedBaud.U)

    // 2) Enable Master
    writeAPB(dut.io.apb, mctrlaAddr.U, 1.U)

    // 3) Wait ~20 cycles for FSM
    dut.clock.step(100)

    // 4) Check toggles
    var toggles = 0
    var prevScl = dut.io.master.scl.peek().litToBoolean

    // We'll step 5000 cycles
    for(_ <- 0 until 4900) {
      dut.clock.step()
      val nowScl = dut.io.master.scl.peek().litToBoolean
      if (nowScl != prevScl) toggles += 1
      prevScl = nowScl
    }
    if (toggles < 10) {
      fail(s"dividerBasicCheck: expected >=10 toggles at 400kHz, saw $toggles toggles!")
    }
  }

  /**
    * Test: dividerRandomCheck
    * 
    * Purpose:
    * - Write random BAUD values to the I2C module.
    * - Ensure that each BAUD write triggers the FSM to update freqReg and halfPeriodReg.
    * - Count the number of SCL toggles over 1000 clock cycles.
    * - Validate that the toggle count matches the expected value based on the BAUD rate.
    */
  def dividerRandomCheck(dut: I2C, params: BaseParams): Unit = {
    implicit val clk: Clock = dut.clock
    dut.clock.setTimeout(0) // Increase timeout to accommodate longer operations

    // Safely retrieve register addresses using getAddressOfRegister
    val mbaudAddrOpt      = dut.registerMap.getAddressOfRegister("mbaud")
    val mctrlaAddrOpt     = dut.registerMap.getAddressOfRegister("mctrla")

    // Ensure all required registers are present
    val mbaudAddr    = mbaudAddrOpt.getOrElse { fail("Register 'mbaud' not found in RegisterMap") }
    val mctrlaAddr   = mctrlaAddrOpt.getOrElse { fail("Register 'mctrla' not found in RegisterMap") }

    // Enable Master by writing to mctrla register
    writeAPB(dut.io.apb, mctrlaAddr.U, 1.U)
    println("DEBUG: Master enabled by writing to mctrla.")

    // Read back mctrla to confirm
    val actualMctrla = readAPB(dut.io.apb, mctrlaAddr.U)
    println(s"DEBUG: Read back mctrla = $actualMctrla")
    if (actualMctrla != 1) {
      fail(s"Failed to set mctrla bit 0 => 1. Read back $actualMctrla")
    }

    // Wait sufficient cycles for FSM to initialize
    dut.clock.step(100)
    println("DEBUG: Waited 20 cycles for FSM to initialize.")

    // Generate random BAUD values and perform the test
    val random = new Random()
    for(i <- 0 until 5) {
      // Generate a random BAUD value between 1 and 200
      val randomBaud = 1 + random.nextInt(200)
      println(s"\n--- Iteration $i: Writing BAUD=$randomBaud ---")

      // 1. Write the new BAUD value to mbaud register
      writeAPB(dut.io.apb, mbaudAddr.U, randomBaud.U)
      println(s"DEBUG: Wrote BAUD=$randomBaud to mbaud register.")

      // Read back mbaud to confirm
      val actualBaud = readAPB(dut.io.apb, mbaudAddr.U)
      println(s"DEBUG: Read back mbaud = $actualBaud")
      if (actualBaud != randomBaud) {
        fail(s"Failed to set mbaud to $randomBaud. Read back $actualBaud")
      }

      // 2. Step 2 clock cycles to allow 'mbaudChanged' to be detected by FSM
      dut.clock.step(2)
      println("DEBUG: Stepped 2 cycles to allow mbaudChanged to be detected.")

      // 3. Step additional 10 cycles to allow the divider FSM to process DIV_FREQ and DIV_PER
      dut.clock.step(100)

      // 4. Calculate the expected number of SCL toggles in 1000 cycles
      //    Each toggle occurs every 'halfPeriodReg' cycles
      //    Thus, expected toggles = 1000 / halfPeriodReg
      val expectedHalfPeriod = 5 + randomBaud
      val expectedToggles = Math.floor(1000.0 / expectedHalfPeriod.toDouble).toInt
      println(s"DEBUG: Expected toggles in 1000 cycles: $expectedToggles")

      // 5. Step 1000 cycles and count actual SCL toggles
      var toggles = 0
      var prevScl = dut.io.master.scl.peek().litToBoolean
      for(_ <- 0 until 1000) {
        dut.clock.step()
        val nowScl = dut.io.master.scl.peek().litToBoolean
        if (nowScl != prevScl) {
          toggles += 1
        }
        prevScl = nowScl
      }
      println(s"DEBUG: Actual toggles in 1000 cycles: $toggles")

      // 6. Validate the toggle count within a tolerance
      val tolerance = 2 // Allow a tolerance of +/-2 toggles
      if (toggles < (expectedToggles - tolerance) || toggles > (expectedToggles + tolerance)) {
        fail(s"dividerRandomCheck: With BAUD=$randomBaud, expected ~$expectedToggles toggles, saw $toggles toggles.")
      } else {
        println(s"PASS: With BAUD=$randomBaud, SCL toggled $toggles times (expected ~$expectedToggles).")
      }
    }
  }

}

