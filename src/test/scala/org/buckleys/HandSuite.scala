package org.buckleys
import org.scalatest.FunSuite
import org.buckleys.Main._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HandSuite extends FunSuite {
  
  trait TestHands {
    val fourOfAKind = new Hand("AD AC AS AH 2D")
    val lowHighCard = new Hand("3D 5C 6H 9D 4D")
    val lowpair = new Hand("4S 4H 5S 7D KH")
    val lowpair2 = new Hand("4C 4D 9S 10D QC")
    val highpair = new Hand("JS JC 2D 3C 5S")
    val highHighCard = new Hand("JD AS 2H QC 5S")
    val highflush = new Hand("JS AS 3S 2S 5S")
    val lowflush = new Hand("2H 5H 6H 9H 3H")
    val straight = new Hand("3D 4D 5C 6S 7D")
    val straightflush = new Hand("3D 4D 5D 6D 7D")
    val fullHouseLow = new Hand("3D 3C 3S QC QS")
    val fullHouseHigh = new Hand("4C 4D 4S 9C 9H")
    val twoPairLow = new Hand("KS KC QS QD AS")
    val twoPairHigh = new Hand("AS AD 2C 2S 3H")
     
  }
  
  test("handType") {
     new TestHands {
       assert(fourOfAKind.handType === "Four of a kind")
     }
  }
  
  test("rankComp") {
    new TestHands {
      println(twoPairHigh.handType)
      assert(highflush > lowflush)
      assert(lowpair2 < lowpair)
      assert(highpair > lowpair)
      //println(fourOfAKind.handRank, straight.handRank)
      assert(highHighCard > lowHighCard)
      
      assert(straight > highpair)
      assert(fourOfAKind > straight)
      assert(fullHouseHigh > fullHouseLow)
      assert(fullHouseLow < straightflush)
      assert(fullHouseHigh > highflush)
      assert(twoPairHigh > twoPairLow)
    }
  }
}