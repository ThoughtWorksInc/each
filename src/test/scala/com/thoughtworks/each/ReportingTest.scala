package com.thoughtworks.each

import com.thoughtworks.each.Monadic._
import org.junit.{Assert, Test}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scalaz.Free._
import scalaz._
import scalaz.syntax.traverse._
import scalaz.std.list._


class ReportingTest {

  private trait AppAction[A]

  private case object GetEmailList extends AppAction[Throwable \/ List[String]]

  private case class GetContactNameByEmail(email: String) extends AppAction[Throwable \/ String]

  private type FreeCommand[A] = FreeC[AppAction, A]

  private type Script[A] = EitherT[FreeCommand, Throwable, A]

  private def toScript[A](action: AppAction[Throwable \/ A]): Script[A] = {
    new Script[A](Free.liftFC(action))
  }

  import scala.language.higherKinds

  import scala.language.implicitConversions

  private implicit def cast[From, To](from: Script[From])(implicit view: From => To): Script[To] = {
    Monad[Script].map[From, To](from)(view)
  }

  private implicit def freeMonadC[S[_]]: Monad[({type f[x] = FreeC[S, x]})#f] = {
    Free.freeMonad[({type f[x] = Coyoneda[S, x]})#f]
  }

  private def eachScript: Script[xml.Elem] = throwableMonadic[Script] {
    val emailList = toScript(GetEmailList).each
    <html>
      <body>
        <table>{
          (for {
            email: String <- emailList.monadicLoop
            if email.matches( """[a-z.\-_]+@[a-z.\-_]+""")
          } yield {
            <tr>
              <td>
                {toScript(GetContactNameByEmail(email)).each}
              </td>
              <td>
                {email}
              </td>
            </tr>
          }).toList
        }</table>
      </body>
    </html>
  }

  private def rawScript: Script[xml.Elem] = {
    toScript(GetEmailList).flatMap { emailList =>
      emailList.traverseM[Script, xml.Elem] { email =>
        toScript(GetContactNameByEmail(email)).map { name =>
          if (email.matches( """[^@]+@[^@]+""")) {
            List(<tr>
              <td>
                {name}
              </td>
              <td>
                {email}
              </td>
            </tr>)
          } else {
            Nil
          }
        }
      }.map { trs =>
        <html>
          <body>
            <table>
              {trs}
            </table>
          </body>
        </html>
      }
    }
  }

  @Test
  def testReporting(): Unit = {
    val Data = Map(
      "atryyang@thoughtworks.com" -> "Yang Bo",
      "invalid-mail-address" -> "N/A",
      "john.smith@gmail.com" -> "John Smith"
    )
    val interpreter = new (AppAction ~> scalaz.Id.Id) {
      override def apply[A](fa: AppAction[A]): A = {
        fa match {
          case GetEmailList => {
            \/-(Data.keys.toList)
          }
          case GetContactNameByEmail(email) =>Data.get(email) match {
            case None => {
              -\/(new NoSuchElementException)
            }
            case Some(name) => {
              \/-(name)
            }
          }
        }
      }
    }

    import scala.concurrent.ExecutionContext.Implicits.global
    import scalaz.std.scalaFuture._

    val rawHtml =
      xml.Xhtml.toXhtml(xml.Utility.trim(Free.runFC(rawScript.run)(interpreter).fold(throw _, identity)))

    val eachHtml =
      xml.Xhtml.toXhtml(xml.Utility.trim(Free.runFC(eachScript.run)(interpreter).fold(throw _, identity)))

    Assert.assertEquals(rawHtml, eachHtml)

  }

  @Test
  def testAsyncReporting(): Unit = {
    val Data = Map(
      "atryyang@thoughtworks.com" -> "Yang Bo",
      "invalid-mail-address" -> "N/A",
      "john.smith@gmail.com" -> "John Smith"
    )
    val interpreter = new (AppAction ~> Future) {
      override def apply[A](fa: AppAction[A]): Future[A] = {
        fa match {
          case GetEmailList => {
            Future.successful(\/-(Data.keys.toList))
          }
          case GetContactNameByEmail(email) => Future.successful(Data.get(email) match {
            case None => {
              -\/(new NoSuchElementException)
            }
            case Some(name) => {
              \/-(name)
            }
          })
        }
      }
    }

    import scala.concurrent.ExecutionContext.Implicits.global
    import scalaz.std.scalaFuture._

    val rawHtml =
      xml.Xhtml.toXhtml(xml.Utility.trim(Await.result(Free.runFC(rawScript.run)(interpreter), Duration.Inf).fold(throw _, identity)))

    val eachHtml =
      xml.Xhtml.toXhtml(xml.Utility.trim(Await.result(Free.runFC(eachScript.run)(interpreter), Duration.Inf).fold(throw _, identity)))

    Assert.assertEquals(rawHtml, eachHtml)

  }

}
