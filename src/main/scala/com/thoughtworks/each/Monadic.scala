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
import scalaz.Bind

final class Monadic[F[_]] {
  /**
   * @usecase def apply[F[_], X](inputTree: X)(implicit monad: scalaz.Monad[F]): F[X] = ???
   * @usecase def apply[F[_], X](inputTree: X)(implicit monadCatchIo: scalaz.effect.MonadCatchIO[F]): F[X] = ???
   */
  def apply[X](inputTree: X)(implicit bind: scalaz.Bind[F]): F[X] = macro Monadic.Macro.monadic

}

object Monadic {

  implicit final class EachOps[F[_], A](val underlying: F[A])(implicit bind: Bind[F]) {
    @compileTimeOnly("`each` must be inside `monadic`.")
    def each: A = ???
  }

  /**
   * @usecase def monadic[F[_], X](inputTree: X)(implicit monad: scalaz.Monad[F]): F[X] = ???
   * @usecase def monadic[F[_], X](inputTree: X)(implicit monadCatchIo: scalaz.effect.MonadCatchIO[F]): F[X] = ???
   */
  def monadic[F[_]] = new Monadic[F]

  private[Monadic] object Macro {

    def monadic(c: scala.reflect.macros.whitebox.Context)(inputTree: c.Tree)(bind: c.Tree): c.Tree = {
      import c.universe._
      //    c.info(c.enclosingPosition, showRaw(inputTree), true)
      val Apply(Apply(TypeApply(Select(monadicTree, _), List(asyncValueTypeTree)), _), _) = c.macroApplication

      val eachOpsType = typeOf[_root_.com.thoughtworks.each.Monadic.EachOps[({type T[F[_]] = {}})#T, _]]
      val eachMethodSymbol = eachOpsType.member(TermName("each"))
      //      val eachOpsType = thisTree.tpe.baseType()
      //    c.info(c.enclosingPosition, show(fType), true)
      val transformer = new MonadicTransformer[c.universe.type](c.universe) {
        override def freshName(name: String) = c.freshName(name)

        override val awaitExtractor: PartialFunction[Tree, Tree] = {
          case eachMethodTree@Select(monadTree, _) if eachMethodTree.symbol == eachMethodSymbol => {
            Select(monadTree, TermName("underlying"))
          }
        }

        override val fType = monadicTree.tpe.widen.typeArgs(0)
      }
      val result = transformer.transform(inputTree)
      //      c.info(c.enclosingPosition, show(result.monad), true)
      c.untypecheck(result)
    }
  }

}