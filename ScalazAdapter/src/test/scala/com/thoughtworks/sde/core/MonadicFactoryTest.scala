package com.thoughtworks.sde.core

import com.thoughtworks.enableIf
import org.junit.Assert._
import org.junit.Test

import scalaz.{Id, Monad, Applicative}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class MonadicFactoryTest {

  implicit final class ApplicativeIdOps[A](val a: A) {
    def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
  }

  @Test
  def testEmptyMonadic(): Unit = {
    assertEquals(0, MonadicFactory[Id.Id](0))
  }

  @Test
  def testOption(): Unit = {
    assertEquals(Some(0), MonadicFactory[Option] {
      MonadicFactory.Instructions.each(Some(0))
    })
  }
  @Test
  def testSeq(): Unit = {
    assertEquals(Seq(0), MonadicFactory[Seq] {
      MonadicFactory.Instructions.each(List(0))
    })
  }

}
