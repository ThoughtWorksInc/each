package com.thoughtworks.scalazMonadFactory

import org.junit.{Assert, Test}

class ComprehensionBindTest {

  @Test
  def testOptionMap(): Unit = {
    val optionBind = ComprehensionBind[Option]
    Assert.assertEquals(
      Option("hello, applicative"), optionBind.map(Option("hello, ")) { a =>
        a + "applicative"
      })
  }

  @Test
  def testOptionBind(): Unit = {
    val optionBind = ComprehensionBind[Option]
    Assert.assertEquals(
      Option("hello, applicative"),
      optionBind.bind(Option("hello, ")) { a =>
        Option(a + "applicative")
      })
  }
  
  @Test
  def testSeqMap(): Unit = {
    val seqBind = ComprehensionBind[Seq]
    Assert.assertEquals(
      Seq("hello, applicative"), seqBind.map(Seq("hello, ")) { a =>
        a + "applicative"
      })
  }

  @Test
  def testSeqBind(): Unit = {
    val seqBind = ComprehensionBind[Seq]
    Assert.assertEquals(
      Seq("hello, applicative"),
      seqBind.bind(Seq("hello, ")) { a =>
        Seq(a + "applicative")
      })
  }

}
