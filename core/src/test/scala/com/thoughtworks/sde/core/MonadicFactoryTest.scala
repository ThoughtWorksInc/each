package com.thoughtworks.sde.core

import org.junit.Assert._
import org.junit.Test

import scalaz.{Id, Monad}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class MonadicFactoryTest {

  @Test
  def testEmptyMonadic(): Unit = {
    assertEquals(0, MonadicFactory[Monad, Id.Id].apply(0))
  }


  @Test
  def testWhiteboxEmptyMonadic(): Unit = {
    assertEquals(0, MonadicFactory.Whitebox[Monad, Id.Id].apply(0))
  }

}
