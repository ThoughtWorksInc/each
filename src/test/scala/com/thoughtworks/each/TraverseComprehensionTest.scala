package com.thoughtworks.each

import org.junit.{Assert, Test}
import Monadic._
import scalaz.syntax.traverse._
import scalaz.std.option._
import scalaz.std.iterable._

class TraverseComprehensionTest {

  @Test
  def testForeach(): Unit = {
    val n = Some(10)
    val result = monadic[Option] {
      var count = 1
      for (i <- Iterable(300, 20).monadicLoop) {
        count += i * n.each
      }
      count
    }
    Assert.assertEquals(Some(3201), result)
  }

  @Test
  def testMap(): Unit = {
    val n = Some(4000)
    val result = monadic[Option] {
      (for (i <- Iterable(300, 20).toEphemeralStream.monadicLoop) yield {
        i + n.each
      }).to[Iterable]
    }
    Assert.assertEquals(Some(Iterable(4300, 4020)), result)
  }

  @Test
  def testFlatMap(): Unit = {
    val n = Some(4000)
    val result = monadic[Option] {
      (for {
        i <- Iterable(300, 20).toEphemeralStream.monadicLoop
        j <- Iterable(50000, 600000).toEphemeralStream.monadicLoop
      } yield {
          i + j + n.each
        }).to[Iterable]
    }
    Assert.assertEquals(Some(Iterable(54300, 604300, 54020, 604020)), result)
  }

  @Test
  def testFilter(): Unit = {
    val n = Some(4000)
    val result = monadic[Option] {
      (for {
        i <- Iterable(300, 20).toEphemeralStream.monadicLoop
        if i > 100
      } yield {
          i + n.each
        }).to[Iterable]
    }
    Assert.assertEquals(Some(Iterable(4300)), result)
  }

//  @Test
//  def testComplex(): Unit = {
//    val n = Some(4000)
//    val result = monadic[Option] {
//      (for {
//        i <- Iterable(300, 20).toEphemeralStream.monadicLoop
//        (j, k) <- Iterable(50000->"1111", 600000->"yyy").toEphemeralStream.monadicLoop
//        if i > 100
//        a = i + k
//      } yield {
//          a + n.each * k.length
//        }).to[Iterable]
//    }
//    Assert.assertEquals(Some(Iterable(16300, 12300)), result)
//  }

}
