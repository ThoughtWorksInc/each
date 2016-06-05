package com.thoughtworks

import com.thoughtworks.generator.Generator
import org.scalatest.{FunSuite, Matchers}
import scala.language.higherKinds

import scalaz.Free._
import scalaz.{-\/, Applicative, Free, MonadPlus, Monoid, Traverse, \/-}
import scalaz.std.list._
import scalaz.std.tuple._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class GeneratorSuite extends FunSuite with Matchers {

  implicit object UnitSourceInstance extends MonadPlus[Source[?, Unit]] {

    private type F[A] = Source[A, Unit]

    override def bind[A, B](fa: F[A])(f: (A) => F[B]): F[B] = {
      fa.resume match {
        case -\/((head, tailSource)) => {
          f(head).flatMap { _: Unit =>
            bind(tailSource)(f)
          }
        }
        case \/-(a) =>
          Free.point[(B, ?), Unit](())
      }
    }

    override def empty[A]: F[A] = {
      Free.point[(A, ?), Unit](())
    }

    override def plus[A](a: F[A], b: => F[A]): F[A] = {
      a.flatMap { _: Unit => b }
    }

    override def point[A](a: => A): F[A] = {
      Free.produce(a)
    }
  }

  // This type class is not available on current scalaz version. See https://github.com/scalaz/scalaz/pull/1160
  implicit def sourceTraverse[A0]: Traverse[Source[?, A0]] = {
    type F[A] = Source[A, A0]
    new Traverse[F] {
      def traverseImpl[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = {
        fa.resume match {
          case -\/((head, tailSource)) => {
            implicitly[Applicative[G]].apply2(f(head), traverseImpl[G, A, B](tailSource)(f)) { (mappedHead: B, mappedTail: F[B]) =>
              Free.produce(mappedHead).flatMap { _: Unit =>
                mappedTail
              }
            }
          }
          case \/-(a) =>
            implicitly[Applicative[G]].point(Free.point[(B, ?), A0](a))
        }
      }

    }
  }


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
      1.gen
      2.gen
    }
    g shouldBe Generator(1, 2)

  }

  test("generator call another generator") {

    @generator[Int] def g: Generator[Int] = {

      @generator[Int] def internal = {
        100.gen
        200.gen
      }

      1.gen
      internal.gen
      2.gen
    }
    g shouldBe Generator(1, 100, 200, 2)

  }


  test("List.foreach") {

    @generator[Int] def g: Generator[Int] = {

      1.gen
      List("100", "200").foreach { s: String => s.toInt.gen }
      2.gen
    }
    g shouldBe Generator(1, 100, 200, 2)

  }


  test("Source.foreach") {

    @generator[Int] def g: Generator[Int] = {

      @generator[String] def internal = {
        "100".gen
        "200".gen
      }
      1.gen
      internal.foreach { s: String => s.toInt.gen }
      2.gen
    }
    g shouldBe Generator(1, 100, 200, 2)

  }

  test("List.filter") {

    @generator[Int] def g: Generator[Int] = {

      1.gen
      for (s <- List("100", "200") if s != "100") {
        s.toInt.gen
      }
      2.gen
    }
    g shouldBe Generator(1, 200, 2)

  }

  test("List.flatMap") {

    @generator[Int] def g: Generator[Int] = {

      1.gen
      val l = for {
        s <- List("100", "200")
        if s != "100"
        i <- List(1, 2)
      } yield {
        s.toInt.gen
        s"$s-$i"
      }
      l shouldBe List("200-1", "200-2")
      2.gen
    }
    g shouldBe Generator(1, 200, 200, 2)

  }

  // Disabled due to lack of Unapply
  //  test("Source.filter") {
  //
  //    @generator[Int] def g: Generator[Int] = {
  //
  //      @generator[String] def internal = {
  //        "100".gen
  //        "200".gen
  //      }
  //      1.gen
  //      for (s <- internal if s != "100") {
  //        s.toInt.gen
  //      }
  //      2.gen
  //    }
  //    g shouldBe Generator(1, 200, 2)
  //
  //  }

  // TODO: Traversable type class for Source itself

}

