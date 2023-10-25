package com.thoughtworks.each

import org.junit.{Assert, Test}
import Monadic._
import scalaz.std.option._
import scalaz.std.list._

class TraverseComprehensionTest {

  @Test
  def testAnnotationForeach(): Unit = {
    import scalaz.std.iterable._
    val n = Some(10)
    @monadic[Option]
    val result = {
      var count = 1
      for (i <- 1 to 10) {
        count += i * n.each
      }
      count
    }
    Assert.assertEquals(Some(551), result)
  }

  @Test
  def testForeach(): Unit = {
    val n = Some(10)
    val result = monadic[Option] {
      var count = 1
      for (i <- List(300, 20).monadicLoop) {
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
      (for (i <- List(300, 20).monadicLoop) yield {
        i + n.each
      }).underlying
    }
    Assert.assertEquals(Some(List(4300, 4020)), result)
  }

  @Test
  def testMapWithAnotherType(): Unit = {
    val result = monadic[Option] {
      (for (i <- List("foo", "bar-baz").monadicLoop) yield {
        i.length
      }).underlying
    }
    Assert.assertEquals(Some(List(3, 7)), result)
  }

  @Test
  def testFlatMap(): Unit = {
    val n = Some(4000)
    val result = monadic[Option] {
      (for {
        i <- List(300, 20).monadicLoop
        j <- List(50000, 600000).monadicLoop
      } yield {
        i + j + n.each
      }).underlying
    }
    Assert.assertEquals(Some(List(54300, 604300, 54020, 604020)), result)
  }

}
