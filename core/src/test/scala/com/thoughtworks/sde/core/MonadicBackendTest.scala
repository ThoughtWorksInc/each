package com.thoughtworks.sde.core

import com.thoughtworks.enableIf
import com.thoughtworks.sde.core.MonadicBackend.ExceptionHandlingMode.{MonadCatchIoMode, MonadThrowableMode, UnsupportedExceptionHandlingMode}
import org.junit.Assert._
import org.junit.Test

import scalaz.{Id, Monad}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class MonadicBackendTest {

  @enableIf(!scala.util.Properties.versionNumberString.startsWith("2.10."))
  @Test
  def testEval(): Unit = {
    import reflect.runtime.universe._
    import tools.reflect.ToolBox
    val tb = reflect.runtime.currentMirror.mkToolBox()
    assertEquals(0, tb.eval(
      q"""
        import com.thoughtworks.sde.core.MonadicBackend.ExceptionHandlingMode.UnsupportedExceptionHandlingMode
        import com.thoughtworks.sde.core.MonadicBackend
        import scalaz.{Id, Monad}
        MonadicBackend.run[Monad, Id.Id, Int](Monad[Id.Id], UnsupportedExceptionHandlingMode, 0)
      """
    ))
  }

  @Test
  def testEmptyMonadic(): Unit = {
    assertEquals(0, MonadicBackend.run[Monad, Id.Id, Int](Monad[Id.Id], UnsupportedExceptionHandlingMode, 0))
  }

}
