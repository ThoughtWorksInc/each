package com.thoughtworks.sde

import com.thoughtworks.sde.source.SourceSeq
import org.scalatest.{FunSuite, Matchers}

import scala.language.higherKinds

import scalaz.Free
import scalaz.std.list._
import scalaz.std.tuple._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class SourceSuite extends FunSuite with Matchers {

  test("block API") {
    source.sourceToSeq(source[Int] {
      source.AutoImports.yieldAll(1, 2)
      source.AutoImports.yieldAll(3, 4)
    }) shouldBe SourceSeq(1, 2, 3, 4)
  }

  test("empty source") {

    type N = Nothing

    @source[N] def emptySourceMethod: SourceSeq[Nothing] = ()
    @source[N] val emptySourceVal: Free.Source[Nothing, Unit] = ()
    val s: SourceSeq[Nothing] = emptySourceVal

    emptySourceMethod shouldBe SourceSeq.empty[Nothing]
    source.sourceToSeq(emptySourceVal) shouldBe SourceSeq.empty[Nothing]
    s shouldBe SourceSeq.empty[Nothing]

  }

  test("simple source") {
    @source[Int] def g: SourceSeq[Int] = {
      1.yieldOne
      2.yieldOne
    }
    g shouldBe SourceSeq(1, 2)
  }

  test("yield all") {
    @source[Int] def g: SourceSeq[Int] = {
      yieldAll(1, 2)
    }
    g shouldBe SourceSeq(1, 2)
  }

  test("source call another source") {

    @source[Int] def g: SourceSeq[Int] = {

      @source[Int] def internal = {
        100.yieldOne
        200.yieldOne
      }

      1.yieldOne
      internal.yieldAll
      2.yieldOne
    }
    g shouldBe SourceSeq(1, 100, 200, 2)

  }

  test("List.foreach") {

    @source[Int] def g: SourceSeq[Int] = {

      1.yieldOne
      List("100", "200").foreach { s: String => s.toInt.yieldOne }
      2.yieldOne
    }
    g shouldBe SourceSeq(1, 100, 200, 2)

  }


  test("Source.foreach") {

    @source[Int] def g: SourceSeq[Int] = {

      @source[String] def internal = {
        "100".yieldOne
        "200".yieldOne
      }
      1.yieldOne
      internal.foreach { s: String => s.toInt.yieldOne }
      2.yieldOne
    }
    g shouldBe SourceSeq(1, 100, 200, 2)

  }

  test("List.filter") {

    @source[Int] def g: SourceSeq[Int] = {

      1.yieldOne
      for (s <- List("100", "200") if s != "100") {
        s.toInt.yieldOne
      }
      2.yieldOne
    }
    g shouldBe SourceSeq(1, 200, 2)

  }

  test("List.flatMap") {
    @source[Int] def g: SourceSeq[Int] = {

      1.yieldOne
      val l = for {
        s <- List("100", "200")
        if s != "100"
        i <- List(1, 2)
      } yield {
        s.toInt.yieldOne
        s"$s-$i"
      }
      l shouldBe List("200-1", "200-2")
      2.yieldOne
    }
    g shouldBe SourceSeq(1, 200, 200, 2)

  }

  test("Source.filter") {

    @source[Int] def g: SourceSeq[Int] = {
      @source[String] def internal = {
        "100".yieldOne
        "200".yieldOne
      }
      1.yieldOne
      for (s <- internal if s != "100") {
        s.toInt.yieldOne
      }
      2.yieldOne
    }
    g shouldBe SourceSeq(1, 200, 2)

  }

  test("throw exceptions") {
    @source[Int] def g: SourceSeq[Int] = {
      "xx".toInt
    }
    intercept[NumberFormatException](g)
  }

}
