package com.bitcoin

import org.scalatest.junit.JUnitSuite
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import scala.collection.SortedMap

class BitCoinTest extends JUnitSuite {

  @Test def testValidHashCode() {
    assertEquals(false, BitCoinMining.checkIfValidBitCoin("nishant", 3));
    assertEquals(true, BitCoinMining.checkIfValidBitCoin("0000dfhjvbsdvsfdvkhjsdfwoyvajbdv", 3));
  }

  @Test def testHashCode() {
    var calString = "nishantmehta.nkdnd2j66ydWorker0"
    assertEquals("00000057ae04b6e2e867d3466be9b6a6fbeff3863cc91e0dd599df0a499158c8", BitCoinMining.hex_digest(calString))

  }
}