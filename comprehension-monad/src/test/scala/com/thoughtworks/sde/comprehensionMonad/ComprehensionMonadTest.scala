package com.thoughtworks.sde.comprehensionMonad

import org.junit.{Assert, Test}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class ComprehensionMonadTest {

  @Test
  def testListPoint(): Unit = {
    val seqApplicative = ComprehensionMonad.comprehensionMonad[List]
    Assert.assertEquals(List("hello, applicative"), seqApplicative.point("hello, applicative"))
  }

  @Test
  def testSeqPoint(): Unit = {
    val seqApplicative = ComprehensionMonad.comprehensionMonad[Seq]
    Assert.assertEquals(Seq("hello, applicative"), seqApplicative.point("hello, applicative"))
  }

  @Test
  def testSeqAp(): Unit = {
    val seqApplicative = ComprehensionMonad.comprehensionMonad[Seq]
    Assert.assertEquals(Seq("Hello1!", "Hello2!", "Hello1?", "Hello2?"), seqApplicative.ap(Seq("Hello1", "Hello2"))(Seq((_: String) + "!", (_: String) + "?")))
  }

  @Test
  def testOptionMap(): Unit = {
    val optionBind = ComprehensionMonad.comprehensionMonad[Option]
    Assert.assertEquals(
      Option("hello, applicative"), optionBind.map(Option("hello, ")) { a =>
        a + "applicative"
      })
  }

  @Test
  def testOptionBind(): Unit = {
    val optionBind = ComprehensionMonad.comprehensionMonad[Option]
    Assert.assertEquals(
      Option("hello, applicative"),
      optionBind.bind(Option("hello, ")) { a =>
        Option(a + "applicative")
      })
  }

  @Test
  def testSeqMap(): Unit = {
    val seqBind = ComprehensionMonad.comprehensionMonad[Seq]
    Assert.assertEquals(
      Seq("hello, applicative"), seqBind.map(Seq("hello, ")) { a =>
        a + "applicative"
      })
  }


  @Test
  def testSeqBind(): Unit = {
    val seqBind = ComprehensionMonad.comprehensionMonad[Seq]
    Assert.assertEquals(
      Seq("hello, applicative"),
      seqBind.bind(Seq("hello, ")) { a =>
        Seq(a + "applicative")
      })
  }

}
