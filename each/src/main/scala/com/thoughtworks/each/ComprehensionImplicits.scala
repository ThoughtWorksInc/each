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

import com.thoughtworks.sde.core.ComprehensionMonadGenerator

import scala.language.experimental.macros
import scala.language.higherKinds
import scalaz._

/**
 * Contains implicit methods to work with types support `for` comprehension.
 */
object ComprehensionImplicits {

  /**
   * Returns a monad implemented by `for` comprehension syntax.
   *
   * `F` must support `for` comprehension, containing `map` and `flatMap` methods,
   * and `F`'s companion object must contains an `apply` method to create a `F[A]` instance from any `A`.
   *
   * @tparam F the higher kinded type that the monad works with, e.g. a Scala container `Seq`.
   * @return the monad.
   */
  implicit def comprehensionMonad[F[_]]: Monad[F] = macro MacroImplementation.comprehensionMonad

  private object MacroImplementation {
    def comprehensionMonad(c: scala.reflect.macros.whitebox.Context): c.Tree = {
      import c.universe._
      val TypeApply(_, List(fTypeTree: TypeTree)) = c.macroApplication
      val fSymbol = fTypeTree.tpe.typeSymbol.asType
      ComprehensionMonadGenerator.generatorMonad[c.universe.type](c.universe, c.freshName)(fSymbol)
    }
  }

}
