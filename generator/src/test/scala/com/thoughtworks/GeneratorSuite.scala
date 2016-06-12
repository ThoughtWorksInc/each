package com.thoughtworks

import com.thoughtworks.generator.Generator
import org.scalatest.{FunSuite, Matchers}

import scala.language.higherKinds
import scalaz.Free._
import scalaz.{-\/, Applicative, Free, Leibniz, MonadPlus, Monoid, Traverse, Unapply, \/-}
import scalaz.std.list._
import scalaz.std.tuple._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class GeneratorSuite extends FunSuite with Matchers {

  test("empty generator") {

    type N = Nothing

    @generator[N] def emptyGeneratorMethod: Generator[Nothing] = ()
    @generator[N] val emptyGeneratorVal: Free.Source[Nothing, Unit] = ()
    val generator: Generator[Nothing] = emptyGeneratorVal

    emptyGeneratorMethod shouldBe Generator.empty[Nothing]
    Generator.sourceToGenerator(emptyGeneratorVal) shouldBe Generator.empty[Nothing]
    generator shouldBe Generator.empty[Nothing]

  }

  test("simple generator") {
    @generator[Int] def g: Generator[Int] = {
      1.yieldOne
      2.yieldOne
    }
    g shouldBe Generator(1, 2)
  }

  test("yield all") {
    @generator[Int] def g: Generator[Int] = {
      yieldAll(1, 2)
    }
    g shouldBe Generator(1, 2)
  }

  test("generator call another generator") {

    @generator[Int] def g: Generator[Int] = {

      @generator[Int] def internal = {
        100.yieldOne
        200.yieldOne
      }

      1.yieldOne
      internal.yieldAll
      2.yieldOne
    }
    g shouldBe Generator(1, 100, 200, 2)

  }

  test("List.foreach") {

    @generator[Int] def g: Generator[Int] = {

      1.yieldOne
      List("100", "200").foreach { s: String => s.toInt.yieldOne }
      2.yieldOne
    }
    g shouldBe Generator(1, 100, 200, 2)

  }


  test("Source.foreach") {

    @generator[Int] def g: Generator[Int] = {

      @generator[String] def internal = {
        "100".yieldOne
        "200".yieldOne
      }
      1.yieldOne
      internal.foreach { s: String => s.toInt.yieldOne }
      2.yieldOne
    }
    g shouldBe Generator(1, 100, 200, 2)

  }

  test("List.filter") {

    @generator[Int] def g: Generator[Int] = {

      1.yieldOne
      for (s <- List("100", "200") if s != "100") {
        s.toInt.yieldOne
      }
      2.yieldOne
    }
    g shouldBe Generator(1, 200, 2)

  }

  test("List.flatMap") {
    @generator[Int] def g: Generator[Int] = {

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
    g shouldBe Generator(1, 200, 200, 2)

  }

  test("Source.filter") {

    @generator[Int] def g: Generator[Int] = {
      @generator[String] def internal = {
        "100".yieldOne
        "200".yieldOne
      }
      1.yieldOne
      for (s <- internal if s != "100") {
        s.toInt.yieldOne
      }
      2.yieldOne
    }
    g shouldBe Generator(1, 200, 2)

  }

  test("throw exceptions") {
    @generator[Int] def g: Generator[Int] = {
      "xx".toInt
    }
    intercept[NumberFormatException](g)
  }

}
