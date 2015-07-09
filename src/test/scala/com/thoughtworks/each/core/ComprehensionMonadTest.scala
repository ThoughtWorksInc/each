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

package com.thoughtworks.each.core

import org.junit.{Assert, Test}

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


  @Test
  def testSeqBind(): Unit = {
    val seqBind = ComprehensionMonad[Seq]
    Assert.assertEquals(
      Seq("hello, applicative"),
      seqBind.bind(Seq("hello, ")) { a =>
        Seq(a + "applicative")
      })
  }

}
