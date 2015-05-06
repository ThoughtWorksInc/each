package com.thoughtworks.scalazMonadFactory

import org.junit.Test
import scalaz.Monad
import scalaz.Bind
import scalaz.Applicative
import scalaz.effect.MonadCatchIO
import scalaz.effect.IO

class TransformerTest {

  @Test
  def test(): Unit = {

    import scalaz.std.option._

    val transformer = Transformer[Option]
    import transformer._
    val s = Some(Nil)

    async {
      {
        val a = s.length
        val b = a + 1
        a + b + Some(b)
      }
      val b = 2
      1 + b
    }
  }

  def test2(): Unit = {

    import scalaz.std.option._

    val transformer = Transformer[Option]
    import transformer._
    val s = Some(Nil)
    val some2 = async {
      if (s.length != 0) {
        val nothing = s(2)
        val nothing2 = (s(2): Int) + (nothing: Int)
        s.length + s.length + (None: Option[Nil.type]).length
      }
      val nothing = s(2)
    }

  }

  def testCatch3(): Unit = {
    implicit def aaaa: MonadCatchIO[Option] = ???

    val transformer = Transformer[Option]
    val s = Some(Nil)

    Bind[Option].bind(
      Bind[Option].ifM(Bind[Option].map(s)(
        ((parameter$macro$12: scala.collection.immutable.Nil.type) => parameter$macro$12.length.$bang$eq(0))),
        MonadCatchIO.ensuring(
          Bind[Option].map(s)(((parameter$macro$13: scala.collection.immutable.Nil.type) =>
            parameter$macro$13.apply(2))),
          Bind[Option].map(s)(((parameter$macro$14: scala.collection.immutable.Nil.type) =>
            parameter$macro$14.apply(3)))),
        Applicative[Option].point(())))(((parameter$macro$15: Unit) => {
        parameter$macro$15;
        Bind[Option].bind(s)(((parameter$macro$16: scala.collection.immutable.Nil.type) => {
          val nothing: Nothing = parameter$macro$16.apply(2); Applicative[Option].point(())
        }))
      }))
  }

  def testCatch(): Unit = {
    val s: IO[String] = ???

    val transformer = Transformer[IO]
    import transformer._
    val some2 = async {
      if (s.length != 0) {
        try {
          s.charAt(2)
        } catch {
          case e: RuntimeException => {
            s.charAt(3)
            ()
          }
          case e: Exception => {
            s.charAt(4)
            ()
          }
        } finally {
          s.charAt(3)

        }
      }
      val nothing = s.charAt(2)
    }

  }

  /* Disable since it is not implemented yet
  def testWhile(): Unit = {

    val transformer = Transformer[Option]
    import transformer._
    val s = Some(0.5)
    val some2 = async {
      while(math.random < s) {
        println("Hello, World!")
      }
    }
  }
  */
  
}

 