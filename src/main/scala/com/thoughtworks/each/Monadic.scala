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

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import scalaz.effect.MonadCatchIO
import scalaz.{Bind, Monad, Unapply}

object Monadic {

  /**
   * The temporary wrapper that contains the `each` method.
   *
   * @param underlying the underlying monadic value.
   * @tparam F the higher kinded type of the monadic value.
   * @tparam A the element type of of the monadic value.
   */
  implicit final class EachOps[F[_], A](val underlying: F[A]) {

    /**
     * Semantically, returns the result in the monadic value.
     *
     * This macro must be inside a `monadic`
     * or a `catchIoMonadic`  block.
     *
     * This is not a real method, thus it will never actually execute.
     * Instead, the method will be transformed to monadic expressions.
     * The actually result is passing as a parameter to some [[scalaz.Monad#bind]] and [[scalaz.Monad#point]] calls
     * instead of as a return value.
     *
     * @return the result in the monadic value.
     */
    @compileTimeOnly("`each` must be inside `monadic` or `catchIoMonadic`.")
    def each: A = ???

  }

  /**
   * An implicit view to enable `.each` for a monadic value.
   *
   * @param v the monadic value.
   * @param F0 a helper to infer types.
   * @tparam FA type of the monadic value.
   * @return the temporary wrapper that contains the `each` method.
   */
  implicit def toEachOpsUnapply[FA](v: FA)(implicit F0: Unapply[Bind, FA]) = new EachOps[F0.M, F0.A](F0(v))


  /**
   * @usecase def monadic[F[_]](body: AnyRef)(implicit monad: Monad[F]): F[body.type] = ???
   *
   *          Captures all the result in the `body` and converts them into a `F`.
   *
   *          Note that `body` must not contain any `try` / `catch` / `throw` expressions.
   *
   * @tparam F the higher kinded type of the monadic expression.
   * @param body the imperative style expressions that will be transform to monadic style.
   * @param monad the monad that executes expressions in `body`.
   * @return
   */
  def monadic[F[_]] = new PartialAppliedMonadic[Monad, F]

  /**
   * @usecase def catchIoMonadic[F[_]](body: AnyRef)(implicit monad: MonadCatchIO[F]): F[body.type] = ???
   *
   *          Captures all the result in the `body` and converts them into a `F`.
   *
   *          Note that `body` may contain any `try` / `catch` / `throw` expressions.
   *
   * @tparam F the higher kinded type of the monadic expression.
   * @param body the imperative style expressions that will be transform to monadic style.
   * @param monad the monad that executes expressions in `body`.
   * @return
   */
  def catchIoMonadic[F[_]] = new PartialAppliedMonadic[MonadCatchIO, F]

  /**
   * Partial applied function instance to convert a monadic expression.
   *
   * For type inferring only.
   *
   * @tparam M
   * @tparam F
   */
  final class PartialAppliedMonadic[M[_[_]], F[_]]() {

    def apply[X](body: X)(implicit monad: M[F]): F[X] = macro PartialAppliedMonadic.MacroImplementation.apply

  }

  private object PartialAppliedMonadic {

    private[PartialAppliedMonadic] object MacroImplementation {

      def apply(c: scala.reflect.macros.whitebox.Context)(body: c.Tree)(monad: c.Tree): c.Tree = {
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
            case eachMethodTree@Select(eachOpsTree, _) if eachMethodTree.symbol == eachMethodSymbol => {
              val actualFType = eachOpsTree.tpe.typeArgs(0)
              val resultType = eachMethodTree.tpe
              val expectedType = appliedType(fType, List(resultType))
              val actualType = appliedType(actualFType, List(resultType))
              if (!(actualType <:< expectedType)) {
                c.error(
                  eachOpsTree.pos,
                  raw"""type mismatch;
 found   : ${show(actualType)}
 required: ${show(expectedType)}""")
              }
              Select(eachOpsTree, TermName("underlying"))
            }
          }

        }
        val result = transformer.transform(body, monad)
        //      c.info(c.enclosingPosition, show(result), true)
        c.untypecheck(result)
      }
    }

  }

}