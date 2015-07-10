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

import com.thoughtworks.each.core.MonadicTransformer

import scala.annotation.{elidable, compileTimeOnly}
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import scalaz.effect.MonadCatchIO
import scalaz.{Monad, Unapply, Bind}

final class Monadic[M[_[_]], F[_]] {

  def apply[X](body: X)(implicit monad: M[F]): F[X] = macro Monadic.Macro.monadic

}

object Monadic {

  implicit final class EachOps[F[_], A](val self: F[A])(implicit bind: Bind[F]) {
    @compileTimeOnly("`each` must be inside `monadic`.")
    def each: A = ???
  }

  implicit def toEachOpsUnapply[FA](v: FA)(implicit F0: Unapply[Bind, FA]) = new EachOps[F0.M, F0.A](F0(v))(F0.TC)

  def monadic[F[_]] = new Monadic[Monad, F]

  def catchIoMonadic[F[_]] = new Monadic[MonadCatchIO, F]

  private[Monadic] object Macro {

    def monadic(c: scala.reflect.macros.whitebox.Context)(body: c.Tree)(monad: c.Tree): c.Tree = {
      import c.universe._
      //    c.info(c.enclosingPosition, showRaw(inputTree), true)
      val Apply(Apply(TypeApply(Select(monadicTree, _), List(asyncValueTypeTree)), _), _) = c.macroApplication

      val ExistentialType(List(widecard), TypeRef(_, eachOpsSymbol, List(_, widecardRef))) =
        typeOf[_root_.com.thoughtworks.each.Monadic.EachOps[({type T[F[_]] = {}})#T, _]]

      val transformer = new MonadicTransformer[c.universe.type](c.universe) {

        override def freshName(name: String) = c.freshName(name)

        override val fType = monadicTree.tpe.widen.typeArgs(1)

        val expectedEachOpsType = {
          internal.existentialType(List(widecard), appliedType(eachOpsSymbol, List(fType, widecardRef)))
        }

        val eachMethodSymbol = expectedEachOpsType.member(TermName("each"))

        override val eachExtractor: PartialFunction[Tree, Tree] = {
          case eachMethodTree@Select(monadTree, _)
            if eachMethodTree.symbol == eachMethodSymbol && monadTree.tpe <:< expectedEachOpsType => {
            Select(monadTree, TermName("self"))
          }
        }

      }
      val result = transformer.transform(body, monad)
      //      c.info(c.enclosingPosition, show(result), true)
      c.untypecheck(result)
    }
  }

}