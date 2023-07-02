/*
Copyright 2015 ThoughtWorks, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package com.thoughtworks.each

import org.junit.{Assert, Test}

class ComprehensionImplicitsTest {

  @Test
  def testListPoint(): Unit = {
    val seqApplicative = ComprehensionImplicits.comprehensionMonad[List]
    Assert.assertEquals(List("hello, applicative"), seqApplicative.point("hello, applicative"))
  }

  @Test
  def testSeqPoint(): Unit = {
    val seqApplicative = ComprehensionImplicits.comprehensionMonad[Seq]
    Assert.assertEquals(Seq("hello, applicative"), seqApplicative.point("hello, applicative"))
  }

  @Test
  def testSeqAp(): Unit = {
    val seqApplicative = ComprehensionImplicits.comprehensionMonad[Seq]
    Assert.assertEquals(Seq("Hello1!", "Hello2!", "Hello1?", "Hello2?"), seqApplicative.ap(Seq("Hello1", "Hello2"))(Seq((_: String) + "!", (_: String) + "?")))
  }

  @Test
  def testOptionMap(): Unit = {
    val optionBind = ComprehensionImplicits.comprehensionMonad[Option]
    Assert.assertEquals(
      Option("hello, applicative"), optionBind.map(Option("hello, ")) { a =>
        a + "applicative"
      })
  }

  @Test
  def testOptionBind(): Unit = {
    val optionBind = ComprehensionImplicits.comprehensionMonad[Option]
    Assert.assertEquals(
      Option("hello, applicative"),
      optionBind.bind(Option("hello, ")) { a =>
        Option(a + "applicative")
      })
  }

  @Test
  def testSeqMap(): Unit = {
    val seqBind = ComprehensionImplicits.comprehensionMonad[Seq]
    Assert.assertEquals(
      Seq("hello, applicative"), seqBind.map(Seq("hello, ")) { a =>
        a + "applicative"
      })
  }


  @Test
  def testSeqBind(): Unit = {
    val seqBind = ComprehensionImplicits.comprehensionMonad[Seq]
    Assert.assertEquals(
      Seq("hello, applicative"),
      seqBind.bind(Seq("hello, ")) { a =>
        Seq(a + "applicative")
      })
  }

}
