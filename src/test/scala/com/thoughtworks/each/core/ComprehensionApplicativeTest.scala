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

import org.junit._

class ComprehensionApplicativeTest {

  @Test
  def testListPoint(): Unit = {
    val seqApplicative = ComprehensionApplicative[List]
    Assert.assertEquals(List("hello, applicative"), seqApplicative.point("hello, applicative"))
  }

  @Test
  def testSeqPoint(): Unit = {
    val seqApplicative = ComprehensionApplicative[Seq]
    Assert.assertEquals(Seq("hello, applicative"), seqApplicative.point("hello, applicative"))
  }

  @Test
  def testSeqAp(): Unit = {
    val seqApplicative = ComprehensionApplicative[Seq]
    Assert.assertEquals(Seq("Hello1!", "Hello1?", "Hello2!", "Hello2?"), seqApplicative.ap(Seq("Hello1", "Hello2"))(Seq((_: String) + "!", (_: String) + "?")))
  }

}
