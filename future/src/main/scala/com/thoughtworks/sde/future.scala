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

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.macros.{blackbox, whitebox}
import scala.language.experimental.macros
import scalaz._
import scalaz.Id.Id
import scala.language.higherKinds
import macrocompat.bundle
import com.thoughtworks.sde.core.Preprocessor

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
final class future(implicit val executionContext: ExecutionContext) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro future.AnnotationBundle.macroTransform
}

// TODO: EitherT, OptionT, ... 似乎可以通过 Instruction 实现
object future {

  type MonadThrowable[F[_]] = MonadError[F, Throwable]

  @bundle
  private[future] final class AnnotationBundle(context: whitebox.Context) extends Preprocessor(context) {

    import c.universe._

    def macroTransform(annottees: Tree*): Tree = {
      replaceDefBody(annottees, { body =>
        q"""
          new _root_.com.thoughtworks.sde.core.MonadicFactory[
            _root_.com.thoughtworks.sde.future.MonadThrowable,
            _root_.scala.concurrent.Future
          ].apply {
            import _root_.com.thoughtworks.sde.future.AutoImports._
            ${(new Virtualizer).transform(body)}
          }(${
            c.macroApplication match {
              case q"new $annotationClass()($executionContext).macroTransform(..$annottees)" =>
                q"_root_.scalaz.std.scalaFuture.futureInstance($executionContext)"
              case q"new $annotationClass($executionContext).macroTransform(..$annottees)" =>
                q"_root_.scalaz.std.scalaFuture.futureInstance($executionContext)"
              case q"new $annotationClass().macroTransform(..$annottees)" =>
                q"_root_.scalaz.std.scalaFuture.futureInstance"
            }
          })
        """
      })
    }

  }

  @bundle
  private[future] class AwaitBundle(val c: whitebox.Context) {
    import c.universe._

    def prefixAwait(future: Tree): Tree = {
      val q"$await[$a]($future)" = c.macroApplication
      q"""
        _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
          _root_.scala.concurrent.Future,
          $a
        ]($future)
      """
    }

    def postfixAwait: Tree = {
      val q"$ops.await" = c.macroApplication
      val opsName = TermName(c.freshName("ops"))
      q"""
        val $opsName = $ops
        _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
          _root_.scala.concurrent.Future,
          $opsName.A
        ]($opsName.underlying)
      """
    }

  }

  object AutoImports {

    import scala.language.implicitConversions

    implicit final class AwaitOps[A0](val underlying: Future[A0]) extends AnyVal {
      type A = A0
      def await: A = macro AwaitBundle.postfixAwait
    }

    implicit def await[A](future: Future[A]): A = macro AwaitBundle.prefixAwait

  }

}
