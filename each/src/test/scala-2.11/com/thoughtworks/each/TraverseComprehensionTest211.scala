package com.thoughtworks.each

import com.thoughtworks.each.Monadic._
import org.junit.{Assert, Test}

import scalaz.std.list._
import scalaz.std.option._

class TraverseComprehensionTest211 {

  @Test
  def testFilter(): Unit = {
    val n = Some(4000)

    val result = monadic[Option] {
      (for {
        i <- List(300, 20).monadicLoop
        if i > 100
      } yield {
        i + n.each
      }).underlying
    }
    Assert.assertEquals(Some(List(4300)), result)
  }

  @Test
  def testComplex(): Unit = {
    val n = Some(4000)
    val result = monadic[Option] {
      (for {
        i <- List(300, 20).monadicLoop
        (j, k) <- List(50000 -> "1111", 600000 -> "yyy").monadicLoop
        if i > n.each - 3900
        a = i + j
      } yield {
        a + n.each * k.length
      }).underlying
    }

    Assert.assertEquals(Some(List(66300, 612300)), result)
  }

}
