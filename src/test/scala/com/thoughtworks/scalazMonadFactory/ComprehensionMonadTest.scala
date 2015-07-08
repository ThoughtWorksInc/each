package com.thoughtworks.scalazMonadFactory

import org.junit.{Assert, Test}

/**
 * Created by longyang.zhang on 7/8/15.
 */
class ComprehensionMonadTest {
  @Test
  def testSeqPoint(): Unit = {
    val seqMonad = ComprehensionMonad[Seq]
    Assert.assertEquals(Seq("hello, monad"), seqMonad.point("hello, monad"))
  }

  @Test
  def testListPoint(): Unit = {
    val listMonad = ComprehensionMonad[List]
    Assert.assertEquals(List("hello, monad"), listMonad.point("hello, monad"))
  }

//  @Test
//  def testSeqBind(): Unit = {
//    val seqMonad = ComprehensionMonad[Seq]
//    Assert.assertTrue(Seq("Hello1!", "Hello1?", "Hello2!", "Hello2?") == seqMonad.bind(Seq("Hello1", "Hello2"))(Seq((_: String) + "!", (_: String) + "?")))
//  }
}
