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

import com.thoughtworks.sde.core.{MonadicFactory, Preprocessor}
import macrocompat.bundle

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.macros._
import scalaz._
import scalaz.effect.MonadCatchIO
import scalaz.syntax.{FoldableOps, MonadPlusOps, TraverseOps}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object Monadic {


  @inline
  implicit final class ToMonadicLoopOps[F[_], A](underlying: F[A]) {

    def monadicLoop = new MonadicLoop(underlying)

  }

  @inline
  implicit def getUnderlying[F[_], A](monadicLoop: MonadicLoop[F, A]): F[A] = monadicLoop.underlying

  object MonadicLoop {

    @bundle
    private[MonadicLoop] final class MacroBundle(val c: blackbox.Context) {

      import c.universe._

      def foreach(f: Tree)(foldable: Tree): Tree = {
        val q"$monadicLoop.foreach[$u]($f)($foldable)" = c.macroApplication
        val monadicLoopName = TermName(c.freshName("monadicLoop"))
        q"""
          val $monadicLoopName = $monadicLoop
          _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.foreach[
            $monadicLoopName.F,
            $monadicLoopName.Element,
            $u
          ]($monadicLoopName.underlying, $foldable, $f)
        """
      }

      def map(f: Tree)(traverse: Tree): Tree = {
        val q"$monadicLoop.map[$b]($f)($traverse)" = c.macroApplication
        val monadicLoopName = TermName(c.freshName("monadicLoop"))
        q"""
          val $monadicLoopName = $monadicLoop
          new _root_.com.thoughtworks.each.Monadic.MonadicLoop[$monadicLoopName.F, $b](
            _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.map[
              $monadicLoopName.F,
              $monadicLoopName.Element,
              $b
            ]($monadicLoopName.underlying, $traverse, $f)
          )
        """
      }

      def flatMap(f: Tree)(traverse: Tree, bind: Tree): Tree = {
        val q"$monadicLoop.flatMap[$b]($f)($traverse, $bind)" = c.macroApplication
        val monadicLoopName = TermName(c.freshName("monadicLoop"))
        q"""
          val $monadicLoopName = $monadicLoop
          new _root_.com.thoughtworks.each.Monadic.MonadicLoop[$monadicLoopName.F, $b](
            _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.flatMap[
              $monadicLoopName.F,
              $monadicLoopName.Element,
              $b
            ]($monadicLoopName.underlying, $traverse, $bind, $f)
          )
        """
      }

      def filter(f: Tree)(traverse: Tree, monadPlus: Tree): Tree = {
        val q"$monadicLoop.${TermName("filter" | "withFilter")}($f)($traverse, $monadPlus)" = c.macroApplication
        val monadicLoopName = TermName(c.freshName("monadicLoop"))
        q"""
          val $monadicLoopName = $monadicLoop
          new _root_.com.thoughtworks.each.Monadic.MonadicLoop[$monadicLoopName.F, $monadicLoopName.Element](
            _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.filter[
              $monadicLoopName.F,
              $monadicLoopName.Element
            ]($monadicLoopName.underlying, $traverse, $monadPlus, $f)
          )
        """
      }

    }

    @inline
    implicit def toFoldableOps[F[_] : Foldable, A](monadicLoop: MonadicLoop[F, A]): FoldableOps[F, A] = {
      scalaz.syntax.foldable.ToFoldableOps(monadicLoop.underlying)
    }

    @inline
    implicit def toTraverseOps[F[_] : Traverse, A](monadicLoop: MonadicLoop[F, A]): TraverseOps[F, A] = {
      scalaz.syntax.traverse.ToTraverseOps(monadicLoop.underlying)
    }

    @inline
    implicit def toMonadPlusOps[F[_] : MonadPlus, A](monadicLoop: MonadicLoop[F, A]): MonadPlusOps[F, A] = {
      scalaz.syntax.monadPlus.ToMonadPlusOps(monadicLoop.underlying)
    }

  }

  @deprecated(
    message = """
      Use `@monadic[X] def f = { ... }` instead of `monadic[X] { ... }`.
      Note that you can remove `.monadicLoop` in `@monadic` methods.
    """,
    since = "1.0.1")
  final class MonadicLoop[F0[_], A](val underlying: F0[A]) {

    type F[X] = F0[X]

    type Element = A

    @inline
    def toFoldableOps(implicit foldable: Foldable[F]) = scalaz.syntax.foldable.ToFoldableOps(underlying)

    @inline
    def toTraverseOps(implicit traverse: Traverse[F]) = scalaz.syntax.traverse.ToTraverseOps(underlying)

    def foreach[U](f: A => U)(implicit foldable: Foldable[F]): Unit = macro MonadicLoop.MacroBundle.foreach

    def map[B](f: A => B)(implicit traverse: Traverse[F]): MonadicLoop[F, B] = macro MonadicLoop.MacroBundle.map

    def flatMap[B](f: A => F[B])(implicit traverse: Traverse[F], bind: Bind[F]): MonadicLoop[F, B] = macro MonadicLoop.MacroBundle.flatMap

    def filter(f: A => Boolean)(implicit traverse: Traverse[F], monadPlus: MonadPlus[F]): MonadicLoop[F, A] = macro MonadicLoop.MacroBundle.filter

    def withFilter(f: A => Boolean)(implicit traverse: Traverse[F], monadPlus: MonadPlus[F]): MonadicLoop[F, A] = macro MonadicLoop.MacroBundle.filter

  }

  /**
    * An implicit view to enable `for` `yield` comprehension for a monadic value.
    *
    * @param v  the monadic value.
    * @param F0 a helper to infer types.
    * @tparam FA type of the monadic value.
    * @return the temporary wrapper that contains the `each` method.
    */
  @inline
  implicit def toMonadicLoopOpsUnapply[FA](v: FA)(implicit F0: Unapply[Foldable, FA]) = {
    new ToMonadicLoopOps[F0.M, F0.A](F0(v))
  }

  object EachOps {

    @bundle
    private[EachOps] final class MacroBundle(val c: whitebox.Context) {

      import c.universe._

      def each: Tree = {
        val q"$ops.each" = c.macroApplication
        val opsName = TermName(c.freshName("ops"))
        q"""
          val $opsName = $ops
          _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
            $opsName.M,
            $opsName.A
          ]($opsName.underlying)
        """
      }
    }

  }

  /**
    * The temporary wrapper that contains the `each` method.
    *
    * @param underlying the underlying monadic value.
    * @tparam M0 the higher kinded type of the monadic value.
    * @tparam A0 the element type of of the monadic value.
    */
  final case class EachOps[M0[_], A0](underlying: M0[A0]) {

    type M[A] = M0[A]

    type A = A0

    /**
      * Semantically, returns the result in the monadic value.
      *
      * This macro must be inside a `monadic`
      * or a `catchIoMonadic`  block.
      *
      * This is not a real method, thus it will never actually execute.
      * Instead, the call to this method will be transformed to a monadic expression.
      * The actually result is passing as a parameter to some [[scalaz.Monad#bind]] and [[scalaz.Monad#point]] calls
      * instead of as a return value.
      *
      * @return the result in the monadic value.
      */
    def each: A = macro EachOps.MacroBundle.each

  }

  /**
    * An implicit view to enable `.each` for a monadic value.
    *
    * @param v  the monadic value.
    * @param F0 a helper to infer types.
    * @tparam FA type of the monadic value.
    * @return the temporary wrapper that contains the `each` method.
    */
  @inline
  implicit def toEachOpsUnapply[FA](v: FA)(implicit F0: Unapply[Bind, FA]): EachOps[F0.M, F0.A] = new EachOps[F0.M, F0.A](F0(v))

  /**
    * An implicit view to enable `.each` for a monadic value.
    *
    * @param v the monadic value.
    * @return the temporary wrapper that contains the `each` method.
    */
  @inline
  implicit def toEachOps[F[_], A](v: F[A]): EachOps[F, A] = new EachOps(v)

  @bundle
  final class AnnotationBundle(context: whitebox.Context) extends Preprocessor(context) {

    import c.universe._


    private def macroTransform(m: Tree, annottees: Seq[Tree]): Tree = {

      val (f, tc) = c.macroApplication match {
        case q"new $annotationClass[$f]()($tc).macroTransform(..$annottees)" =>
          (f, tc)
        case q"new $annotationClass[$f]($tc).macroTransform(..$annottees)" =>
          (f, tc)
        case q"new $annotationClass[$f]().macroTransform(..$annottees)" =>
          (f, q"_root_.scala.Predef.implicitly[$m[$f]]")
      }

      val eachOpsName = TermName(c.freshName("eachOps"))
      val toEachOpsName = TermName(c.freshName("ToEachOps"))

      replaceDefBody(annottees, { body =>
        q"""
          _root_.com.thoughtworks.sde.core.MonadicFactory[
            $m,
            $f
          ].apply {
            object $toEachOpsName {
              import scala.language.implicitConversions
              implicit def $eachOpsName[A](fa: $f[A]): _root_.com.thoughtworks.each.Monadic.EachOps[$f, A] = {
                new _root_.com.thoughtworks.each.Monadic.EachOps[$f, A](fa)
              }
            }
            import $toEachOpsName.$eachOpsName
            ${(new ComprehensionTransformer).transform(body)}
          }($tc)
        """
      })
    }

    def throwableMonadic(annottees: Tree*): Tree = {
      macroTransform(tq"_root_.com.thoughtworks.each.Monadic.MonadThrowable", annottees)
    }

    def monadic(annottees: Tree*): Tree = {
      macroTransform(tq"_root_.scalaz.Monad", annottees)
    }

    def catchIoMonadic(annottees: Tree*): Tree = {
      macroTransform(tq"_root_.scalaz.MonadCatchIO", annottees)
    }

  }

  /**
    * @usecase def monadic[F[_]](body: AnyRef)(implicit monad: Monad[F]): F[body.type] = ???
    *
    *          Captures all the result in the `body` and converts them into a `F`.
    *
    *          Note that `body` must not contain any `try` / `catch` / `throw` expressions.
    * @tparam F the higher kinded type of the monadic expression.
    * @param body  the imperative style expressions that will be transform to monadic style.
    * @param monad the monad that executes expressions in `body`.
    * @return
    */
  @inline
  def monadic[F[_]] = MonadicFactory[Monad, F]

  @compileTimeOnly("enable macro paradise to expand macro annotations")
  final class monadic[F[_]] extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro AnnotationBundle.monadic
  }

  /**
    * @usecase def catchIoMonadic[F[_]](body: AnyRef)(implicit monad: MonadCatchIO[F]): F[body.type] = ???
    *
    *          Captures all the result in the `body` and converts them into a `F`.
    *
    *          Note that `body` may contain any `try` / `catch` / `throw` expressions.
    * @tparam F the higher kinded type of the monadic expression.
    * @param body  the imperative style expressions that will be transform to monadic style.
    * @param monad the monad that executes expressions in `body`.
    * @return
    */
  @inline
  def catchIoMonadic[F[_]] = MonadicFactory[MonadCatchIO, F]

  @compileTimeOnly("enable macro paradise to expand macro annotations")
  final class catchIoMonadic[F[_]] extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro AnnotationBundle.catchIoMonadic
  }

  // TODO: create Unapply instead
  @inline
  implicit def eitherTMonadThrowable[F[_], G[_[_], _]](implicit F0: Monad[({type g[y] = G[F, y]})#g]): MonadThrowable[
    ({type f[x] = EitherT[({type g[y] = G[F, y]})#g, Throwable, x]})#f
    ] = {
    EitherT.eitherTMonadError[({type g[y] = G[F, y]})#g, Throwable]
  }

  @inline
  implicit def lazyEitherTMonadThrowable[F[_], G[_[_], _]](implicit F0: Monad[({type g[y] = G[F, y]})#g]): MonadThrowable[
    ({type f[x] = LazyEitherT[({type g[y] = G[F, y]})#g, Throwable, x]})#f
    ] = {
    LazyEitherT.lazyEitherTMonadError[({type g[y] = G[F, y]})#g, Throwable]
  }


  /**
    * A [[scalaz.Monad]] that supports exception handling.
    *
    * Note this is a simplified version of [[scalaz.MonadError]].
    *
    * @tparam F the higher kinded type of the monad.
    */
  type MonadThrowable[F[_]] = MonadError[F, Throwable]

  /**
    * @usecase def throwableMonadic[F[_]](body: AnyRef)(implicit monad: MonadThrowable[F]): F[body.type] = ???
    *
    *          Captures all the result in the `body` and converts them into a `F`.
    *
    *          Note that `body` may contain any `try` / `catch` / `throw` expressions.
    * @tparam F the higher kinded type of the monadic expression.
    * @param body  the imperative style expressions that will be transform to monadic style.
    * @param monad the monad that executes expressions in `body`.
    * @return
    */
  @inline
  def throwableMonadic[F[_]] = MonadicFactory[MonadThrowable, F]

  @compileTimeOnly("enable macro paradise to expand macro annotations")
  final class throwableMonadic[F[_]] extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro AnnotationBundle.throwableMonadic
  }

}
