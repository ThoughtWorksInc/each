/*
Copyright 2016 ThoughtWorks, Inc.

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

import com.thoughtworks.each.Monadic.monadic
import org.junit.{Assert, Test}
import scalaz.std.option._
import scalaz.std.list._

/** @author
  *   杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class AnnotationTest {

  @Test
  def testForeach(): Unit = {
    val n = Some(10)
    @monadic[Option]
    val result = {
      var count = 1
      for (i <- List(300, 20)) {
        count += i * n.each
      }
      count
    }
    Assert.assertEquals(Some(3201), result)
  }

  @Test
  def testMap(): Unit = {
    val n = Some(4000)
    @monadic[Option]
    val result = {
      for (i <- List(300, 20)) yield {
        i + n.each
      }
    }
    Assert.assertEquals(Some(List(4300, 4020)), result)
  }

  @Test
  def testFlatMap(): Unit = {
    val n = Some(4000)
    @monadic[Option]
    val result = {
      for {
        i <- List(300, 20)
        j <- List(50000, 600000)
      } yield {
        i + j + n.each
      }
    }
    Assert.assertEquals(Some(List(54300, 604300, 54020, 604020)), result)
  }

  @Test
  def testFilter(): Unit = {
    val n = Some(4000)

    @monadic[Option]
    val result = {
      for {
        i <- List(300, 20)
        if i > 100
      } yield {
        i + n.each
      }
    }
    Assert.assertEquals(Some(List(4300)), result)
  }
}
