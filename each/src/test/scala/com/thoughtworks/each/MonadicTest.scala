/*
Copyright 2015 ThoughtWorks, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package com.thoughtworks.each

import com.thoughtworks.each.ComprehensionImplicits._
import com.thoughtworks.each.Monadic._
import org.junit.{Assert, Test}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scalaz.{IndexedStateT, Monad}

import scalaz.effect.IO

class MonadicTest {

  @Test
  def testOption(): Unit = {
    def plusOne(intOption: Option[Int]) = monadic[Option] {
      intOption.each + 1
    }
    Assert.assertEquals(None, plusOne(None))
    Assert.assertEquals(Some(16), plusOne(Some(15)))
  }

  @Test
  def testSeq(): Unit = {
    def plusOne(intSeq: Seq[Int]) = monadic[Seq] {
      intSeq.each + 1
    }
    Assert.assertEquals(Seq.empty, plusOne(Seq.empty))
    Assert.assertEquals(Seq(16), plusOne(Seq(15)))
    Assert.assertEquals(Seq(16, -1, 10), plusOne(Seq(15, -2, 9)))
  }

  @Test
  def testFuture(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val f101 = monadic[Future] {
      Future(1).each + Future(100).each
    }
    Assert.assertEquals(101, Await.result(f101, Duration.Inf))
  }

  @Test
  def testPow(): Unit = {
    val pow = monadic[Seq](math.pow(2.0, (0 to 10).each))
    Assert.assertEquals(Seq(1.0, 2.0, 4.0, 8.0, 16.0, 32.0, 64.0, 128.0, 256.0, 512.0, 1024.0), pow)
  }

  @Test
  def testMultiply(): Unit = {
    val result = monadic[Seq]((0 to 3).each * (10 to 13).each)
    Assert.assertEquals(Seq(0, 0, 0, 0, 10, 11, 12, 13, 20, 22, 24, 26, 30, 33, 36, 39), result)
  }

  @Test
  def testIoWhile(): Unit = {

    def s = IO("123")
    var count = 0
    val io = monadic[IO] {
      var i = 0
      while (i < 100) {
        count += s.each.length
        i += 1
      }
      i
    }

    Assert.assertEquals(100, io.unsafePerformIO())

    Assert.assertEquals(300, count)
  }

  @Test
  def testWhile(): Unit = {
    def s = Option("123")
    var count = 0
    monadic[Option] {
      val i = 100
      while (i != 100) {
        count += s.each.length
      }
    }
    Assert.assertEquals(0, count)
  }


  @Test
  def testIf(): Unit = {
    val ifOption = monadic[Option] {
      val i = Option(1)
      val j: Int = if (i.each > 1) 2 else 10
      i.each + j
    }

    Assert.assertEquals(Some(11), ifOption)
  }


  @Test
  def testReturn(): Unit = {

    def returnExprssions(input: Option[Int]): Option[Int] = monadic[Option] {
      if (input.each < 0) {
        return Some(-1)
      }
      if (input.each < 10) {
        return Some(0)
      }
      input.each
    }

    Assert.assertEquals(Some(-1), returnExprssions(Some(-1234)))
    Assert.assertEquals(Some(0), returnExprssions(Some(5)))
    Assert.assertEquals(Some(13), returnExprssions(Some(13)))
    Assert.assertEquals(None, returnExprssions(None))
  }

  @Test
  def testImport(): Unit = {
    object A {
      def currentImport = "A"
    }

    object B {
      def currentImport = "B"
    }

    object C {
      def currentImport = "C"
    }

    val result = monadic[Option] {
      import A._
      Assert.assertEquals("A", currentImport)

      {
        import B._
        Assert.assertEquals("B", currentImport)

        {
          import C._
          Assert.assertEquals("C", currentImport)
        }
      }

      currentImport
    }

    Assert.assertEquals(Some("A"), result)
  }

  @Test
  def testAssignExpressions(): Unit = {
    val assignExp = monadic[Option] {
      var pi = 3.1415
      pi = 1.0
      pi
    }

    Assert.assertEquals(Some(1.0), assignExp)

  }

  @Test
  def testDefDef(): Unit = {

    val lengthOption = monadic[Option] {
      def s = Option(Nil)
      s.each.length
    }

    Assert.assertEquals(Monad[Option].map {
      def s = Option(Nil)
      s
    }(_.length), lengthOption)
  }

  @Test
  def testSomeNilLength(): Unit = {
    val s = Option(Nil)

    val lengthOption = monadic[Option] {
      s.each.length
    }

    Assert.assertEquals(Monad[Option].map(s)(_.length), lengthOption)

  }

  @Test
  def testNoneLength(): Unit = {
    val s: Option[Seq[Nothing]] = None

    val lengthOption = monadic[Option] {
      s.each.length
    }

    Assert.assertEquals(Monad[Option].map(s)(_.length), lengthOption)

  }

  @Test
  def testNewByOption(): Unit = {
    val newS = monadic[Option] {
      new String("a string")
    }

    Assert.assertEquals(Monad[Option].pure(new String("a string")), newS)
    Assert.assertEquals(Some(new String("a string")), newS)
  }

  @Test
  def testNewBySeq(): Unit = {
    val newS = monadic[Seq] {
      new String("a string")
    }

    Assert.assertEquals(Monad[Seq].pure(new String("a string")), newS)
    Assert.assertEquals(Seq(new String("a string")), newS)
  }

  @Test
  def testConcatSeq = {

    val list1 = Seq("foo", "bar", "baz")
    val list2 = Seq("Hello", "World!")

    val concatSeq = monadic[Seq](list1.each.substring(0, 2) + " " + list2.each.substring(1, 4))

    Assert.assertEquals(
      for {
        string1 <- list1
        string2 <- list2
      } yield (string1.substring(0, 2) + " " + string2.substring(1, 4)),
      concatSeq)
    Assert.assertEquals(Seq("fo ell", "fo orl", "ba ell", "ba orl", "ba ell", "ba orl"), concatSeq)
  }

  @Test
  def testConcatSet = {

    val list1 = Set("foo", "bar", "baz")
    val list2 = Set("Hello", "World!")

    val concatSet = monadic[Set](list1.each.substring(0, 2) + " " + list2.each.substring(1, 4))

    Assert.assertEquals(
      for {
        string1 <- list1
        string2 <- list2
      } yield (string1.substring(0, 2) + " " + string2.substring(1, 4)),
      concatSet)
    Assert.assertEquals(Set("fo ell", "fo orl", "ba ell", "ba orl", "ba ell", "ba orl"), concatSet)
  }

  @Test
  def testBlock(): Unit = {
    var count = 0
    val io = monadic[IO] {
      val _ = IO(()).each
      count += 1
      count += 1
      count
    }
    Assert.assertEquals(0, count)
    Assert.assertEquals(2, io.unsafePerformIO())
    Assert.assertEquals(2, count)

  }

  @Test
  def testCatch(): Unit = {
    var count = 0
    val io = catchIoMonadic[IO] {
      val _ = IO(()).each
      try {
        count += 1
        (null: Array[Int])(0)
      } catch {
        case e: NullPointerException => {
          count += 1
          100
        }
      } finally {
        count += 1
      }
    }
    Assert.assertEquals(0, count)
    Assert.assertEquals(100, io.unsafePerformIO())
    Assert.assertEquals(3, count)
  }

  @Test
  def testThrowCatch(): Unit = {
    var count = 0
    val io = catchIoMonadic[IO] {
      val _ = IO(()).each
      try {
        count += 1
        throw new Exception
      } catch {
        case e: Exception => {
          count += 1
          100
        }
      } finally {
        count += 1
      }
    }
    Assert.assertEquals(0, count)
    Assert.assertEquals(100, io.unsafePerformIO())
    Assert.assertEquals(3, count)
  }

  @Test
  def testNestedClass(): Unit = {
    trait Base {
      def bar: Int
    }
    val nestedClass = monadic[Option][Base] {
      class Foo() extends Base {
        def bar = 100
      }
      new Foo
    }

    Assert.assertEquals(100, nestedClass.get.bar)
  }

  @Test
  def testVarIf(): Unit = {
    var count = 0
    def io(initialValue: Int) = monadic[IO] {
      var i = initialValue
      if (i == 0) {
        i = 1
      } else {
        i = 2
      }
      i += 10
      i
    }

    Assert.assertEquals(11, io(0).unsafePerformIO())
    Assert.assertEquals(12, io(-1).unsafePerformIO())

    val state = {
      IndexedStateT.stateTMonadState[Int, IO].ifM(
        IndexedStateT.stateTMonadState[Int, IO].get.map(_ == 0),
        IndexedStateT.stateTMonadState[Int, IO].put(1),
        IndexedStateT.stateTMonadState[Int, IO].put(2)
      ).flatMap { _ =>
        IndexedStateT.stateTMonadState[Int, IO].get
      }.flatMap { v =>
        IndexedStateT.stateTMonadState[Int, IO].put(v + 10)
      }.flatMap { _ =>
        IndexedStateT.stateTMonadState[Int, IO].get
      }
    }

    Assert.assertEquals(state.eval(0).unsafePerformIO(), io(0).unsafePerformIO())
    Assert.assertEquals(state.eval(-1).unsafePerformIO(), io(-1).unsafePerformIO())
  }

  @Test
  def testMatch(): Unit = {

    val optionHead = monadic[Option] {
      (Option(Seq("foo", "bar", "baz")).each match {
        case head :: tail => {
          Some(head)
        }
        case _ => {
          None
        }
      }).each
    }

    Assert.assertEquals(Some("foo"), optionHead)
  }

  @Test
  def testIoDoWhile(): Unit = {
    def s = IO("123")
    var count = 0
    val io = monadic[IO] {
      var i = 0
      do {
        count += s.each.length
        i += 1
      } while (i < 100)
      i
    }

    Assert.assertEquals(100, io.unsafePerformIO())

    Assert.assertEquals(300, count)
  }


  @Test
  def testDoWhile(): Unit = {
    def s = Option("123")
    var count = 0
    val option = monadic[Option] {
      var i = 0
      do {
        count += s.each.length
        i += 1
      } while (i < 0)
      i
    }

    Assert.assertEquals(Some(1), option)

    Assert.assertEquals(3, count)
  }

  @Test
  def testThis(): Unit = {
    import scala.language.existentials
    val thisClass = monadic[Option] {
      this.getClass
    }

    Assert.assertEquals(Some(classOf[MonadicTest]), thisClass)
  }

  @Test
  def testTuple(): Unit = {
    val result = monadic[Option] {
      val (a, b, c) = Some((1, 2, 3)).each
      a + b + c
    }

    Assert.assertEquals(Some(6), result)
  }

  @Test
  def testSuper(): Unit = {
    class Super {
      def foo = "super"
    }

    object Child extends Super {
      override def foo = "child"

      val superFoo = monadic[Option] {
        super.foo
      }
    }

    Assert.assertEquals(Some("super"), Child.superFoo)
  }

  @Test
  def testAnnotation(): Unit = {
    val selector = Seq(1, 2, 3)
    Assert.assertEquals(Some(Seq(1, 2, 3)), monadic[Option] {
      (selector: @unchecked) match {
        case s: Seq[String@unchecked] => {
          s
        }
      }
    })
  }

  @Test
  def testXml(): Unit = {
    val someFoo = Option(<foo bar="1"/>)
    val result = monadic[Option] {
      <baz>
        {someFoo.each}
      </baz>
    }
    Assert.assertEquals(Some(
      <baz>
        <foo bar="1"/>
      </baz>), result)
  }
}

 