package com.thoughtworks.each

import java.io.IOException

import org.junit.{Assert, Test}
import Monadic._
import scalaz.Free.FreeC
import scalaz._
import scala.language.higherKinds
import scala.language.existentials
import scala.language.implicitConversions
import scalaz.std.option._

class MonadicErrorTest {


  @Test
  def testTryCatchOption(): Unit = {

    case object MyException extends Exception

    type OptionScript[A] = EitherT[Option, Throwable, A]

    val either = {
      var count = 0

      import scala.language.implicitConversions
      implicit def cast[From, To](from: OptionScript[From])(implicit view: From => To): OptionScript[To] = {
        Monad[OptionScript].map[From, To](from)(view)
      }

      val either = throwableMonadic[OptionScript] {
        try {
          count += 1
          throw MyException
          count += 10
          throw new Exception("Unreachable code")
        } catch {
          case MyException => {
            count += 100
            count
          }
        } finally {
          count += 1000
        }
      }
      Assert.assertEquals(1101, count)
      either
    }

    Assert.assertEquals(Some(\/-(101)), either.run)
    Assert.assertEquals({
      var count = 0
      val monad$macro$1: MonadThrowable[OptionScript] = Monadic.eitherTMonadThrowable[Option](scalaz.std.option.optionInstance);

      val expectedEither = {
        type F$macro$1[A$macro$11] = OptionScript[A$macro$11];
        {
          val monad$macro$2 = Monadic.eitherTMonadThrowable[Option](scalaz.std.option.optionInstance);
          implicit def cast$macro$4[From$macro$5, To$macro$7](from$macro$6: F$macro$1[From$macro$5])(implicit view$macro$3: Function1[From$macro$5, To$macro$7]): F$macro$1[To$macro$7] = monad$macro$2.map[From$macro$5, To$macro$7](from$macro$6)(view$macro$3);
          monad$macro$2.map(monad$macro$2.handleError[Int](monad$macro$2.handleError[Int]({
            count += 1
            monad$macro$2.bind(monad$macro$2.raiseError[Nothing](MyException))(((element$macro$8: Nothing) => {
              count += 10
              monad$macro$2.raiseError[Nothing](new Exception("Unreachable code"))
            }))
          })(((exception$macro$9: Throwable) => exception$macro$9 match {
            case MyException => {
              count += 100
              monad$macro$2.point(count)
            }
            case _ => monad$macro$2.raiseError[Int](exception$macro$9)
          })))(((exception$macro$9: Throwable) => {
            count += 1000
            monad$macro$2.raiseError[Int](exception$macro$9)
          })))(((element$macro$10: Int) => {
            count += 1000
            element$macro$10
          }))
        }
      }

      Assert.assertEquals(1101, count)
      expectedEither
    }.run, either.run)
  }


  implicit private def freeMonadC[S[_]]: Monad[({type f[x] = FreeC[S, x]})#f] =
    Free.freeMonad[({type f[x] = Coyoneda[S, x]})#f]

  private trait Command[A]

  private case object RandomInt extends Command[Throwable \/ Int]

  private case class Count(delta: Int) extends Command[Throwable \/ Int]

  private type FreeCommand[A] = FreeC[Command, A]

  private type Script[A] = EitherT[FreeCommand, Throwable, A]

  private val randomInt: Script[Int] = EitherT[FreeCommand, Throwable, Int](Free.liftFC(RandomInt))

  private def count(delta: Int): Script[Int] = EitherT[FreeCommand, Throwable, Int](Free.liftFC(Count(delta)))

  private case object MyException extends Exception


  def noScript(randomInt: () => Int) = {
    var count = 0
    count += 50000
    try {
      if (randomInt() > 100) {
        count += 600000
        throw MyException
        count += 7000000
        789
      } else if (randomInt() > 10) {
        count += 1
        123
      } else {
        count += 20
        (throw new IOException): Int
      }
    } catch {
      case e: IOException => {
        count += 300
        456
      }
    } finally {
      count += 4000
    }
  }

  private def newScriptWithoutEach: Script[Int] = {
    import scalaz.syntax.monadError._

    count(50000).flatMap { _ =>
      implicitly[MonadThrowable[Script]].handleError {
        Monad[Script].ifM(
        randomInt map {
          _ > 100
        }, {
          count(600000).flatMap { _ =>
            implicitly[MonadThrowable[Script]].raiseError(MyException) flatMap { _: Nothing =>
              count(7000000).map { _ =>
                789
              }
            }
          }
        }, {
          Monad[Script].ifM(
          randomInt map {
            _ > 100
          }, {
            count(1).map { _ =>
              123
            }
          }, {
            count(20).flatMap { _ =>
              implicitly[MonadThrowable[Script]].raiseError(new IOException) map { x: Nothing =>
                x: Int
              }
            }
          })
        })
      } {
        case e: IOException => {
          count(300).map { _ =>
            456
          }
        }
        case e => {
          count(4000).flatMap { _ =>
            implicitly[MonadThrowable[Script]].raiseError(e)
          }
        }
      }
    }

  }

  private def newScript: Script[Int] = {
    throwableMonadic[Script] {
      count(50000).each
      try {
        if (randomInt.each > 100) {
          count(600000).each
          throw MyException
          count(7000000).each
          789
        } else if (randomInt.each > 10) {
          count(1).each
          123
        } else {
          count(20).each
          (throw new IOException): Int
        }
      } catch {
        case e: IOException => {
          count(300).each
          456
        }
      } finally {
        count(4000).each
      }
    }
  }

  @Test
  def testFreeMyException(): Unit = {
    val script = newScript
    var count = 0
    val result: Throwable \/ Int = Free.runFC(script.run)(new (Command ~> Id.Id) {
      override def apply[A](command: Command[A]): A = {
        command match {
          case Count(delta) => {
            count += delta
            \/-(count)
          }
          case RandomInt => {
            -\/(MyException)
          }
        }
      }
    })
    Assert.assertEquals(\/.fromTryCatchNonFatal(noScript(() => throw MyException)), result)
    Assert.assertEquals(-\/(MyException), result)
    Assert.assertEquals(54000, count)
  }

  @Test
  def testFreeIOException(): Unit = {
    val script = newScript
    var count = 0
    val result: Throwable \/ Int = Free.runFC(script.run)(new (Command ~> Id.Id) {
      override def apply[A](command: Command[A]): A = {
        command match {
          case Count(delta) => {
            count += delta
            \/-(count)
          }
          case RandomInt => {
            -\/(new IOException)
          }
        }
      }
    })
    Assert.assertEquals(\/.fromTryCatchNonFatal(noScript(() => throw new IOException)), result)
    Assert.assertEquals(\/-(456), result)
    Assert.assertEquals(54300, count)
  }

  @Test
  def testFree150(): Unit = {
    val script = newScript
    var count = 0
    val result: Throwable \/ Int = Free.runFC(script.run)(new (Command ~> Id.Id) {
      override def apply[A](command: Command[A]): A = {
        command match {
          case Count(delta) => {
            count += delta
            \/-(count)
          }
          case RandomInt => {
            \/-(150)
          }
        }
      }
    })
    Assert.assertEquals(\/.fromTryCatchNonFatal(noScript(() => 150)), result)
    Assert.assertEquals(-\/(MyException), result)
    Assert.assertEquals(654000, count)
  }

  @Test
  def testFree15(): Unit = {
    val script = newScript
    var count = 0
    val result: Throwable \/ Int = Free.runFC(script.run)(new (Command ~> Id.Id) {
      override def apply[A](command: Command[A]): A = {
        command match {
          case Count(delta) => {
            count += delta
            \/-(count)
          }
          case RandomInt => {
            \/-(15)
          }
        }
      }
    })
    Assert.assertEquals(\/.fromTryCatchNonFatal(noScript(() => 15)), result)
    Assert.assertEquals(\/-(123), result)
    Assert.assertEquals(54001, count)
  }

  @Test
  def testFree5(): Unit = {
    val script = newScript
    var count = 0
    val result: Throwable \/ Int = Free.runFC(script.run)(new (Command ~> Id.Id) {
      override def apply[A](command: Command[A]): A = {
        command match {
          case Count(delta) => {
            count += delta
            \/-(count)
          }
          case RandomInt => {
            \/-(5)
          }
        }
      }
    })
    Assert.assertEquals(\/.fromTryCatchNonFatal(noScript(() =>5)), result)
    Assert.assertEquals(\/-(456), result)
    Assert.assertEquals(54320, count)
  }

}
