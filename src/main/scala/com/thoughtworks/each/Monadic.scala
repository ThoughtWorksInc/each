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
import com.thoughtworks.each.core.MonadicTransformer._

import scala.language.existentials
import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import scalaz.effect.MonadCatchIO
import scalaz._
import scalaz.syntax.{FoldableOps, TraverseOps}

object Monadic {

  final class MonadicLoop[FA](private[Monadic] val underlying: FA)

  implicit final class ToMonadicLoopOps[FA](underlying: FA) {

    def monadicLoop = new MonadicLoop(underlying)

  }

  final class FoldableComprehensionOps[F[_], A](private[Monadic] val underlying: F[A])(implicit F: Foldable[F]) {

    type Element = A

    def toFoldableOps = scalaz.syntax.foldable.ToFoldableOps(underlying)

    @compileTimeOnly("`foreach` must be inside `monadic`, `throwableMonadic`, or `catchIoMonadic`.")
    def foreach[U](f: A => U): Unit = ???

  }

  final class TraverseComprehensionOps[F[_], A](private[Monadic] val underlying: F[A])(implicit private[Monadic] val F: Traverse[F]) {

    type Element = A

    def toTraverseOps = scalaz.syntax.traverse.ToTraverseOps(underlying)

    @compileTimeOnly("`map` must be inside `monadic`, `throwableMonadic`, or `catchIoMonadic`.")
    def map[B](f: A => B): F[B] = ???

    @compileTimeOnly("`flatMap` must be inside `monadic`, `throwableMonadic`, or `catchIoMonadic`.")
    def flatMap[B](f: A => F[B])(implicit bind: Bind[F]): F[B] = ???


  }


  final class MonadPlusComprehensionOps[F[_], A](private[Monadic] val underlying: F[A])(implicit private[Monadic] val F: MonadPlus[F]) {
    def withFilter(p: A => Boolean): MonadicLoop[F[A]] = {
      new MonadicLoop(F.filter(underlying)(p))
    }
  }


  implicit def monadPlusComprehensionOpsToFoldableComprehensionOpsTo[F[_], A](v: MonadPlusComprehensionOps[F, A])(implicit F: Foldable[F]) = {
    new FoldableComprehensionOps(v.underlying)
  }

  implicit def monadPlusComprehensionOpsToTraverseComprehensionOpsTo[F[_], A](v: MonadPlusComprehensionOps[F, A])(implicit F: Traverse[F]) = {
    new TraverseComprehensionOps(v.underlying)
  }

  implicit def foldableComprehensionOpsToTraverseComprehensionOpsTo[F[_], A](v: FoldableComprehensionOps[F, A])(implicit F: Traverse[F]) = {
    new TraverseComprehensionOps(v.underlying)
  }


  implicit def foldableComprehensionOpsToMonadPlusComprehensionOps[F[_], A](v: FoldableComprehensionOps[F, A])(implicit F: MonadPlus[F]) = {
    new MonadPlusComprehensionOps(v.underlying)
  }

  implicit def traverseComprehensionOpsToFoldableComprehensionOps[F[_], A](v: TraverseComprehensionOps[F, A]) = {
    import v._
    new FoldableComprehensionOps(underlying)
  }


  implicit def traverseComprehensionOpsToMonadPlusComprehensionOps[F[_], A](v: TraverseComprehensionOps[F, A])(implicit F: MonadPlus[F]) = {
    new MonadPlusComprehensionOps(v.underlying)
  }

  /**
   * An implicit view to enable `for` `yield` comprehension for a monadic value.
   *
   * @param v the monadic value.
   * @param F0 a helper to infer types.
   * @tparam FA type of the monadic value.
   * @return the temporary wrapper that contains the `each` method.
   */
  implicit def monadicLoopToFoldableComprehensionOps[FA](v: MonadicLoop[FA])(implicit F0: Unapply[Foldable, FA]) = {
    new FoldableComprehensionOps[F0.M, F0.A](F0(v.underlying))(F0.TC)
  }

  /**
   * An implicit view to enable `for` `yield` comprehension for a monadic value.
   *
   * @param v the monadic value.
   * @param F0 a helper to infer types.
   * @tparam FA type of the monadic value.
   * @return the temporary wrapper that contains the `each` method.
   */
  implicit def monadicLoopToTraverseComprehensionOpsUnapply[FA](v: MonadicLoop[FA])(implicit F0: Unapply[Traverse, FA]) = {
    new TraverseComprehensionOps[F0.M, F0.A](F0(v.underlying))(F0.TC)
  }


  /**
   * An implicit view to enable `for` `yield` comprehension for a monadic value.
   *
   * @param v the monadic value.
   * @param F0 a helper to infer types.
   * @tparam FA type of the monadic value.
   * @return the temporary wrapper that contains the `each` method.
   */
  implicit def monadicLoopToMonadPlusComprehensionOpsUnapply[FA](v: MonadicLoop[FA])(implicit F0: Unapply[MonadPlus, FA]) = {
    new MonadPlusComprehensionOps[F0.M, F0.A](F0(v.underlying))(F0.TC)
  }

  /**
   * An implicit view to enable `for` `yield` comprehension for a monadic value.
   *
   * @param v the monadic value.
   * @param F0 a helper to infer types.
   * @tparam FA type of the monadic value.
   * @return the temporary wrapper that contains the `each` method.
   */
  implicit def toFoldableComprehensionOps[FA](v: FA)(implicit F0: Unapply[Foldable, FA]) = {
    new FoldableComprehensionOps[F0.M, F0.A](F0(v))(F0.TC)
  }

  /**
   * An implicit view to enable `for` `yield` comprehension for a monadic value.
   *
   * @param v the monadic value.
   * @param F0 a helper to infer types.
   * @tparam FA type of the monadic value.
   * @return the temporary wrapper that contains the `each` method.
   */
  implicit def toTraverseComprehensionOpsUnapply[FA](v: FA)(implicit F0: Unapply[Traverse, FA]) = {
    new TraverseComprehensionOps[F0.M, F0.A](F0(v))(F0.TC)
  }

  /**
   * An implicit view to enable `for` `yield` comprehension for a monadic value.
   *
   * @param v the monadic value.
   * @param F0 a helper to infer types.
   * @tparam FA type of the monadic value.
   * @return the temporary wrapper that contains the `each` method.
   */
  implicit def toMonadPlusComprehensionOpsUnapply[FA](v: FA)(implicit F0: Unapply[MonadPlus, FA]) = {
    new MonadPlusComprehensionOps[F0.M, F0.A](F0(v))(F0.TC)
  }

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
     * Instead, the call to this method will be transformed to a monadic expression.
     * The actually result is passing as a parameter to some [[scalaz.Monad#bind]] and [[scalaz.Monad#point]] calls
     * instead of as a return value.
     *
     * @return the result in the monadic value.
     */
    @compileTimeOnly("`each` must be inside `monadic`, `throwableMonadic`, or `catchIoMonadic`.")
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
  def monadic[F[_]] = new PartialAppliedMonadic[Monad, F, UnsupportedExceptionHandlingMode.type]

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
  def catchIoMonadic[F[_]] = new PartialAppliedMonadic[MonadCatchIO, F, MonadCatchIoMode.type]

  implicit def monadErrorToMonadThrowable[F[_, _]](implicit monadError: MonadError[F, Throwable]): MonadThrowable[({type λ[α] = F[Throwable, α]})#λ] = {
    monadError.asInstanceOf[MonadThrowable[({type λ[α] = F[Throwable, α]})#λ]]
  }

  implicit def eitherTMonadThrowable[F[_]](implicit F0: Monad[F]): MonadThrowable[({type f[x] = EitherT[F, Throwable, x]})#f] = {
    val monadError = EitherT.eitherTMonadError[F, Throwable]
    monadErrorToMonadThrowable[({type λ[α, β] = EitherT[F, α, β]})#λ](monadError)
  }

  implicit def lazyEitherTMonadThrowable[F[_]](implicit F0: Monad[F]): MonadThrowable[({type f[x] = LazyEitherT[F, Throwable, x]})#f] = {
    val monadError = LazyEitherT.lazyEitherTMonadError[F, Throwable]
    monadErrorToMonadThrowable[({type λ[α, β] = LazyEitherT[F, α, β]})#λ](monadError)
  }

  /**
   * A [[scalaz.Monad]] that supports exception handling.
   *
   * Note this is a simplified version of [[scalaz.MonadError]].
   *
   * @tparam F the higher kinded type of the monad.
   */
  type MonadThrowable[F[_]] = MonadError[G, Throwable] forSome {type G[Throwable, A] <: F[A]}

  /**
   * @usecase def throwableMonadic[F[_]](body: AnyRef)(implicit monad: MonadThrowable[F]): F[body.type] = ???
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
  def throwableMonadic[F[_]] = new PartialAppliedMonadic[MonadThrowable, F, MonadThrowableMode.type]

  /**
   * Partial applied function instance to convert a monadic expression.
   *
   * For type inferring only.
   *
   * @tparam M
   * @tparam F
   */
  final class PartialAppliedMonadic[M[_[_]], F0[_], Mode <: ExceptionHandlingMode] private[Monadic]() {

    type F[A] = F0[A]

    def apply[X](body: X)(implicit monad: M[F]): F[X] = macro PartialAppliedMonadic.MacroImplementation.apply

  }

  private object PartialAppliedMonadic {

    private[PartialAppliedMonadic] object MacroImplementation {

      def apply(c: scala.reflect.macros.whitebox.Context)(body: c.Tree)(monad: c.Tree): c.Tree = {
        import c.universe._
        //        c.info(c.enclosingPosition, showRaw(c.macroApplication), true)
        val Apply(Apply(TypeApply(Select(partialAppliedMonadicTree, _), List(asyncValueTypeTree)), _), _) = c.macroApplication


        val modeType: Type = partialAppliedMonadicTree.tpe.widen.typeArgs(2)

        val mode: ExceptionHandlingMode = if (modeType =:= typeOf[MonadCatchIoMode.type]) {
          MonadCatchIoMode
        } else if (modeType =:= typeOf[MonadThrowableMode.type]) {
          MonadThrowableMode
        } else if (modeType =:= typeOf[UnsupportedExceptionHandlingMode.type]) {
          UnsupportedExceptionHandlingMode
        } else {
          throw new IllegalStateException("Unsupported ExceptionHandlingMode")
        }
        val partialAppliedMonadicType = partialAppliedMonadicTree.tpe.widen
        val fType = partialAppliedMonadicType.typeArgs(1)

        val transformer = new MonadicTransformer[c.universe.type](c.universe, mode) {

          override def freshName(name: String) = c.freshName(name)

          override def fTree = SelectFromTypeTree(TypeTree(partialAppliedMonadicType), TypeName("F"))

          private val eachMethodSymbol = {
            val eachOpsType = typeOf[_root_.com.thoughtworks.each.Monadic.EachOps[({type T[F[_]] = {}})#T, _]]
            eachOpsType.member(TermName("each"))
          }

          private val foldableComprehensionOpsType = typeOf[_root_.com.thoughtworks.each.Monadic.FoldableComprehensionOps[({type T[F[_]] = {}})#T, _]]

          private val traverseComprehensionOpsType = typeOf[_root_.com.thoughtworks.each.Monadic.TraverseComprehensionOps[({type T[F[_]] = {}})#T, _]]

          private val foreachMethodSymbol = foldableComprehensionOpsType.member(TermName("foreach"))

          private val mapMethodSymbol = traverseComprehensionOpsType.member(TermName("map"))

          private val flatMapMethodSymbol = traverseComprehensionOpsType.member(TermName("flatMap"))

          override val instructionExtractor: PartialFunction[Tree, Instruction] = {
            case Apply(Apply(TypeApply(methodTree@Select(opsTree, _), _), List(bodyFunctionTree: Function)), List(bindTree)) if methodTree.symbol == flatMapMethodSymbol => {
              FlatMap(
                Select(opsTree, TermName("toTraverseOps")),
                bodyFunctionTree,
                bindTree)
            }
            case Apply(TypeApply(methodTree@Select(opsTree, _), _), List(bodyFunctionTree: Function)) if methodTree.symbol == mapMethodSymbol => {
              Map(
                Select(opsTree, TermName("toTraverseOps")),
                bodyFunctionTree)
            }
            case Apply(TypeApply(methodTree@Select(opsTree, _), _), List(bodyFunctionTree: Function)) if methodTree.symbol == foreachMethodSymbol => {
              Foreach(
                Select(opsTree, TermName("toFoldableOps")),
                bodyFunctionTree)
            }
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
              Each(Select(eachOpsTree, TermName("underlying")))
            }
          }

        }
        val result = transformer.transform(body, monad)
        //        c.info(c.enclosingPosition, show(result), true)
        c.untypecheck(result)
      }
    }

  }

}