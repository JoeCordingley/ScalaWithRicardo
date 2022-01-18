package euler

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Euler3Test extends AnyFreeSpec with Matchers {
  "prime factors of 13195 should be 5 7 13 and 29" in {
    Euler3.primeFactors(13195) should contain theSameElementsAs List(5, 7, 13, 29)
  }
}
