package com.thoughtworks.sde

import org.scalacheck.Gen._
import org.scalatest.{FreeSpec, FunSuite, Inside, Matchers}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class GenSuite extends FunSuite with Matchers with Inside {
  test("gen") {
    @gen def genPair = {
      (choose(0.0, 1.0).gen, choose(10, 20).gen)
    }

    inside(genPair.sample) {
      case Some((d: Double, i: Int)) =>
        d should be >= 0.0
        d should be <= 1.0
        i should be >= 10
        i should be <= 20
    }
  }
}
