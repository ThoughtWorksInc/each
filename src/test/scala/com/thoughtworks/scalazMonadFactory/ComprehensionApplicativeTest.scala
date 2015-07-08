package com.thoughtworks.scalazMonadFactory

import org.junit._

class ComprehensionApplicativeTest {

  @Test
  def testSeqPoint(): Unit = {
    val seqApplicative = ComprehensionApplicative[Seq]
    Assert.assertTrue(Seq("hello, applicative") == seqApplicative.point("hello, applicative"))
  }

  @Test
  def testSeqAp(): Unit = {
    val seqApplicative = ComprehensionApplicative[Seq]
    Assert.assertTrue(Seq("Hello1!", "Hello1?", "Hello2!", "Hello2?") == seqApplicative.ap(Seq("Hello1", "Hello2"))(Seq((_: String) + "!", (_: String) + "?")))
  }

}
