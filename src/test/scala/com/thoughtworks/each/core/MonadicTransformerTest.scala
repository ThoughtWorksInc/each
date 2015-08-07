package com.thoughtworks.each.core

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import com.thoughtworks.each.Monadic._
import org.junit.{Assert, Test}
import scala.language.higherKinds

class MonadicTransformerTest {


  @Test
  def testPoint(): Unit = {
    println(reify(1).actualType)
    val transformer = new MonadicTransformer[universe.type](universe) {

      private val EachOpsFunction = {
        val Expr(Apply(eachOpsFunction, List(_))) = reify(EachOps(???))
        eachOpsFunction
      }

      override protected val eachExtractor: PartialFunction[Tree, Tree] = {
        case eachMethodTree@Select(Apply(TypeApply(eachOpsFunction, _), List(self)), TermName("each")) => {
          val e2 = EachOpsFunction
          val e1 = eachOpsFunction
          println(e2 == eachOpsFunction)
          self
        }

      }

      // See https://issues.scala-lang.org/browse/SI-5712
      override protected def fType: Type = ???

      var seed = 1

      override protected def freshName(name: String): String = {
        val id = seed
        seed += 1
        s"$name$$$id"
      }
    }
    val result = transformer.transform((reify(List(1).each)).tree, reify(scalaz.std.list.listInstance).tree)
    println(show(result))


  }
}
