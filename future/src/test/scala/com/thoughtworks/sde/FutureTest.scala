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

package com.thoughtworks.sde

import org.junit.{Assert, Test}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class FutureTest {

  /*
  要实现的功能:


   * 支持 for / yield （不重要）
   * 支持 future 中内嵌的 OptionT, EitherT （应该在类型推导后）
   * 支持 future 中内嵌的 SeqT, ListT, ...
   * 支持 Scala.js
   * 支持运行时反射
   * 支持显式的 .each 和隐式转换两种语法
   * 像 F# 一样,预先封装好所有参数,不需要 import 任何额外模块,就能启用以上功能,包括隐式转换
   * 方法注解

不支持的功能
   * 支持 IntelliJ IDEA 中的类型推断, 怎么做到? 是否要显式定义 await 参数?


如果允许隐式转换,一定要生成特有转换函数
特有转换函数如果隐藏起来,那么会导致IntelliJ IDEA类型推断失效

可以调查一下如果提供全局的隐式转换函数,但是要靠生成的隐式参数来启用,会怎样.

还是想用隐藏的转换函数

要在预处理过程中解决的问题:
map/flatMap/
XML

   */

  @Test
  def testSimpleValue(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    @future
    def async100: Future[Int] = 100

    Assert.assertEquals(100, Await.result(async100, Duration.Inf))
  }

  @Test
  def testImplicitParameter(): Unit = {
    @future
    def async100(implicit ec: ExecutionContext): Future[Int] = 100

    Assert.assertEquals(100, Await.result(async100(ExecutionContext.Implicits.global), Duration.Inf))
  }

  @Test
  def testMath(): Unit = {
    import ExecutionContext.Implicits.global
    @future
    def asyncExpr: Future[Int] = {
      val i = 1 + Future(20) + 300 - await(Future(4000)) + Future(50000).await
      i * 10
    }

    Assert.assertEquals(463210, Await.result(asyncExpr, Duration.Inf))
  }

  @Test
  def testForLoop(): Unit = {
    import ExecutionContext.Implicits.global
    import scalaz.std.list._

    var count = 0
    @future
    def asyncForLoop: Future[Unit] = {
      for (i <- List(Future(1), Future(2), Future(3))) {
        count += i
      }
    }
    Assert.assertEquals((), Await.result(asyncForLoop, Duration.Inf))
    Assert.assertEquals(6, count)
  }

  @Test
  def testComprehension(): Unit = {
    import ExecutionContext.Implicits.global
    import scalaz.std.list._

    @future
    def asyncList: Future[List[Int]] = {
      for {
        i <- List(Future(1), Future(2), Future(3))
      } yield {
        i * 10
      }
    }
    Assert.assertEquals(List(10, 20, 30), Await.result(asyncList, Duration.Inf))
  }

  @Test
  def testForNil(): Unit = {
    import scalaz.std.list._
    import ExecutionContext.Implicits.global

    @future
    def asyncList: Future[List[String]] = {
      for {
        i <- Nil: List[Int]
      } yield {
        i.toString
      }
    }
    Assert.assertEquals(Nil, Await.result(asyncList, Duration.Inf))

  }

  @Test
  def testMultiLineComprehension(): Unit = {
    import ExecutionContext.Implicits.global
    import scalaz.std.list._

    val list0 = List(Future(1), Future(2), Future(3))
    val list1 = List(Future(10), Future(20))
    val list2 = List(3, 5, 7)

    @future
    def asyncList: Future[List[Int]] = {
      for {
        i <- list0
        j <- list1
        k <- list2
        if k < 4 || k > 6
      } yield {
        i * j * k
      }
    }
    Assert.assertEquals(List(30, 70, 60, 140, 60, 140, 120, 280, 90, 210, 180, 420), Await.result(asyncList, Duration.Inf))
  }

  //
  //  @Test
  //  def `testOptionalInAsyncNone`(): Unit = {
  //
  //    @future
  //    def asyncOptional = {
  //      optional(None: String)
  //    }
  //
  //    Assert.assertEquals(None, Await.result(async100, Duration.Inf))
  //  }
  //
  //  @Test
  //  def testOptionalInAsync(): Unit = {
  //
  //    @future
  //    def asyncOptional = {
  //      optional(Some(1000): Int)
  //    }
  //
  //    Assert.assertEquals(Some(1000), Await.result(async100, Duration.Inf))
  //  }
  //
  //  @Test
  //  def `testOptionalInAsyncNone`(): Unit = {
  //
  //    @future
  //    def asyncOptional = optional(None.!)
  //
  //    Assert.assertEquals(None, Await.result(async100, Duration.Inf))
  //  }
  //
  //  @Test
  //  def testOptionalInAsync(): Unit = {
  //
  //    @future
  //    def asyncOptional = optional(Some(1000).!)
  //
  //    Assert.assertEquals(Some(1000), Await.result(async100, Duration.Inf))
  //  }

}
