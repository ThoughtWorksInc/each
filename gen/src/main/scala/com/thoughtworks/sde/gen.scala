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

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scalaz._
import scala.language.higherKinds
import macrocompat.bundle
import com.thoughtworks.sde.core.{MonadicFactory, Preprocessor}
import org.scalacheck.Gen

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
final class gen extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro gen.AnnotationBundle.macroTransform
}

object gen extends MonadicFactory.WithTypeClass[Monad, Gen] {

  override val typeClass = scalaz.scalacheck.ScalaCheckBinding.GenMonad

  @bundle
  private[gen] final class AnnotationBundle(context: whitebox.Context) extends Preprocessor(context) {

    import c.universe._

    def macroTransform(annottees: Tree*): Tree = {
      replaceDefBody(annottees, { body =>
        q"""
          _root_.com.thoughtworks.sde.gen {
            import _root_.com.thoughtworks.sde.gen.AutoImports._
            ${(new ComprehensionTransformer).transform(body)}
          }
        """
      })
    }

  }

  @bundle
  private[gen] class GenBundle(val c: whitebox.Context) {

    import c.universe._

    def postfixGen: Tree = {
      val q"$ops.gen" = c.macroApplication
      val opsName = TermName(c.freshName("ops"))
      q"""
        val $opsName = $ops
        _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
          org.scalacheck.Gen,
          $opsName.A
        ]($opsName.underlying)
      """
    }

  }

  object AutoImports {

    import scala.language.implicitConversions

    implicit final class AwaitOps[A0](val underlying: Gen[A0]) extends AnyVal {
      type A = A0

      def gen: A = macro GenBundle.postfixGen
    }

  }

}
