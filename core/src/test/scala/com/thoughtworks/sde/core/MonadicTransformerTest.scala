package com.thoughtworks.sde.core

import com.thoughtworks.enableIf
import com.thoughtworks.sde.core.MonadicTransformer.ExceptionHandlingMode.{MonadCatchIoMode, MonadThrowableMode, UnsupportedExceptionHandlingMode}
import org.junit.Assert._
import org.junit.Test

import scalaz.{Id, Monad}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class MonadicTransformerTest {

  @enableIf(!scala.util.Properties.versionNumberString.startsWith("2.10."))
  @Test
  def testEval(): Unit = {
    import reflect.runtime.universe._
    import tools.reflect.ToolBox
    val tb = reflect.runtime.currentMirror.mkToolBox()
    assertEquals(0, tb.eval(
      q"""
        import com.thoughtworks.sde.core.MonadicTransformer.ExceptionHandlingMode.UnsupportedExceptionHandlingMode
        import com.thoughtworks.sde.core.MonadicTransformer
        import scalaz.{Id, Monad}
        MonadicTransformer[Monad, Id.Id, Int](Monad[Id.Id], UnsupportedExceptionHandlingMode, 0)
      """
    ))
  }

  @Test
  def testEmptyMonadic(): Unit = {
    assertEquals(0, MonadicTransformer[Monad, Id.Id, Int](Monad[Id.Id], UnsupportedExceptionHandlingMode, 0))
  }

}
