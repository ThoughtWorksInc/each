package com.thoughtworks.sde.core

import com.thoughtworks.enableIf
import org.junit.Assert._
import org.junit.Test

import scalaz.{Id, Monad}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class MonadicFactoryTest {

  /**
   * @note This test is desabled for Scala 2.10 because of https://issues.scala-lang.org/browse/SI-6636
   */
  @enableIf(!scala.util.Properties.versionNumberString.startsWith("2.10."))
  @Test
  def testEval(): Unit = {
    import reflect.runtime.universe._
    import tools.reflect.ToolBox
    val tb = reflect.runtime.currentMirror.mkToolBox()
    assertEquals(0, tb.eval(
      q"""
        import scalaz.{Id, Monad}
        import com.thoughtworks.sde.core.MonadicFactory
        new MonadicFactory[Monad, Id.Id]()(0)
      """
    ))
  }

  @Test
  def testEmptyMonadic(): Unit = {
    assertEquals(0, new MonadicFactory[Monad, Id.Id]()(0))
  }

}
