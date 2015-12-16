package com.thoughtworks.each

import java.io.IOException

import org.junit.{Assert, Test}
import Monadic._
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
  }


  private trait Command[A]

  private case object RandomInt extends Command[Throwable \/ Int]

  private case class Count(delta: Int) extends Command[Throwable \/ Int]

  private type FreeCommand[A] = Free[Command, A]

  private type Script[A] = EitherT[FreeCommand, Throwable, A]

  private val randomInt: Script[Int] = EitherT[FreeCommand, Throwable, Int](Free.liftF(RandomInt))

  private def count(delta: Int): Script[Int] = EitherT[FreeCommand, Throwable, Int](Free.liftF(Count(delta)))

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
    val result: Throwable \/ Int = script.run.foldMap(new (Command ~> Id.Id) {
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
    val result: Throwable \/ Int = script.run.foldMap(new (Command ~> Id.Id) {
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
    val result: Throwable \/ Int = script.run.foldMap(new (Command ~> Id.Id) {
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
    val result: Throwable \/ Int = script.run.foldMap(new (Command ~> Id.Id) {
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
    val result: Throwable \/ Int = script.run.foldMap(new (Command ~> Id.Id) {
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
    Assert.assertEquals(\/.fromTryCatchNonFatal(noScript(() => 5)), result)
    Assert.assertEquals(\/-(456), result)
    Assert.assertEquals(54320, count)
  }

}
