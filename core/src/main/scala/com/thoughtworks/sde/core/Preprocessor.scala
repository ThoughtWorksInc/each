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

package com.thoughtworks.sde.core

import macrocompat.bundle

import scalaz.{Bind, Foldable, MonadPlus, Traverse, Unapply}
import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@bundle
class Preprocessor(val c: whitebox.Context) {

  import c.universe._

  final def replaceDefBody(annottees: Seq[c.universe.Tree], transformBody: Tree => Tree) = {
    val result = annottees match {
      case Seq(annottee@DefDef(mods, name, tparams, vparamss, tpt, rhs)) =>
        atPos(annottee.pos) {
          DefDef(mods, name, tparams, vparamss, tpt, transformBody(rhs))
        }
      case Seq(annottee@ValDef(mods, name, tpt, rhs)) =>
        atPos(annottee.pos) {
          ValDef(mods, name, tpt, transformBody(rhs))
        }
      case _ =>
        c.error(c.enclosingPosition, "Expect def or val")
        annottees.head
    }
    //    c.info(c.enclosingPosition, show(result), true)
    result
  }

  class ComprehensionTransformer extends Transformer {
    override def transform(tree: Tree): Tree = {
      atPos(tree.pos) {
        tree match {
          case q"""$fa.${methodName@TermName("filter" | "withFilter")}(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.core.Preprocessor.Internal.traverseOps(${
              transform(fa)
            }).$methodName(${
              transform(f)
            })"""
          case q"""$fa.map(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.core.Preprocessor.Internal.traverseOps(${
              transform(fa)
            }).map(${
              transform(f)
            })"""
          case q"""$fa.foreach(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.core.Preprocessor.Internal.foldableOps(${
              transform(fa)
            }).foreach(${
              transform(f)
            })"""
          case q"""$fa.flatMap(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.core.Preprocessor.Internal.traverseOps(${
              transform(fa)
            }).flatMap(${
              transform(f)
            })"""
          case _ =>
            super.transform(tree)
        }
      }
    }
  }

}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object Preprocessor {

  object Internal {


    @bundle
    private[Internal] final class MacroBundle(val c: whitebox.Context) {

      import c.universe._

      def filter(f: Tree)(monadPlus: Tree): Tree = {
        val q"$ops.${TermName("filter"|"withFilter")}($f)($monadPlus)" = c.macroApplication
        val opsName = TermName(c.freshName("ops"))
        q"""{
          val $opsName = $ops
          _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.filter[$opsName.M, $opsName.A]($opsName.ma, $opsName.TC, $monadPlus, $f)
        }"""
      }

      def map(f: Tree): Tree = {
        val q"$ops.map[$b]($f)" = c.macroApplication
        val opsName = TermName(c.freshName("ops"))
        q"""{
          val $opsName = $ops
          _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.map[$opsName.M, $opsName.A, $b]($opsName.ma, $opsName.TC, $f)
        }"""
      }

      def flatMap(f: Tree)(bind: Tree): Tree = {
        val q"$ops.flatMap[$b]($f)($bind)" = c.macroApplication
        val opsName = TermName(c.freshName("ops"))
        q"""{
          val $opsName = $ops
          _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.flatMap[$opsName.M, $opsName.A, $b]($opsName.ma, $opsName.TC, $bind, $f)
        }"""
      }

      def foreach(f: Tree): Tree = {
        val q"$ops.foreach[$u]($f)" = c.macroApplication 
        val opsName = TermName(c.freshName("ops"))
        q"""{
          val $opsName = $ops
          _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.foreach[$opsName.M, $opsName.A, $u]($opsName.ma, $opsName.TC, $f)
        }"""
      }

    }

    final case class MacroTraverseOps[M0[_], A0](ma: M0[A0], TC: Traverse[M0]) {

      type M[A] = M0[A]

      type A = A0

      def filter(f: A => Boolean)(implicit monadPlus: MonadPlus[M0]): M0[A] = macro MacroBundle.filter

      def withFilter(f: A => Boolean)(implicit monadPlus: MonadPlus[M0]): M0[A] = macro MacroBundle.filter

      def map[B](f: A => B): M0[B] = macro MacroBundle.map

      def flatMap[B](f: A => M0[B])(implicit bind: Bind[M0]): M0[B] = macro MacroBundle.flatMap

    }

    final case class MacroFoldableOps[M0[_], A0](ma: M0[A0], TC: Foldable[M0]) {

      type M[A] = M0[A]

      type A = A0

      def foreach[U](f: A => U): Unit = macro MacroBundle.foreach

    }

    sealed trait OpsFactory[MA, M[_[_]], Ops] extends (MA => Ops)

    trait FallbackOpsFactory0 {
      @inline
      implicit final def notFound[MA, M[_[_]]]: OpsFactory[MA, M, MA] = new OpsFactory[MA, M, MA] {
        @inline
        override def apply(from: MA): MA = from
      }
    }

    trait FallbackOpsFactory1 extends FallbackOpsFactory0 {

      @inline
      implicit final def macroFoldableOps[F[_], A](implicit foldable: Foldable[F]): OpsFactory[F[A], Foldable, MacroFoldableOps[F, A]] = {
        new OpsFactory[F[A], Foldable, MacroFoldableOps[F, A]] {
          override def apply(fa: F[A]): MacroFoldableOps[F, A] = {
            new MacroFoldableOps[F, A](fa, foldable)
          }
        }
      }

      @inline
      implicit final def macroTraverseOps[F[_], A](implicit traverse: Traverse[F]): OpsFactory[F[A], Traverse, MacroTraverseOps[F, A]] = {
        new OpsFactory[F[A], Traverse, MacroTraverseOps[F, A]] {
          override def apply(fa: F[A]): MacroTraverseOps[F, A] = {
            new MacroTraverseOps[F, A](fa, traverse)
          }
        }
      }

    }

    object OpsFactory extends FallbackOpsFactory1 {

      @inline
      implicit final def macroFoldableOpsUnapply[MA](implicit unapply: Unapply[Foldable, MA]): OpsFactory[MA, Foldable, MacroFoldableOps[unapply.M, unapply.A]] = {
        new OpsFactory[MA, Foldable, MacroFoldableOps[unapply.M, unapply.A]] {
          override def apply(fa: MA): MacroFoldableOps[unapply.M, unapply.A] = {
            new MacroFoldableOps[unapply.M, unapply.A](unapply(fa), unapply.TC)
          }
        }
      }

      @inline
      implicit final def macroTraverseOpsUnapply[MA](implicit unapply: Unapply[Traverse, MA]): OpsFactory[MA, Traverse, MacroTraverseOps[unapply.M, unapply.A]] = {
        new OpsFactory[MA, Traverse, MacroTraverseOps[unapply.M, unapply.A]] {
          override def apply(fa: MA): MacroTraverseOps[unapply.M, unapply.A] = {
            new MacroTraverseOps[unapply.M, unapply.A](unapply(fa), unapply.TC)
          }
        }
      }


    }

    @inline
    def traverseOps[MA, To](fa: MA)(implicit opsFactory: OpsFactory[MA, Traverse, To]): To = {
      opsFactory(fa)
    }

    @inline
    def foldableOps[MA, To](fa: MA)(implicit opsFactory: OpsFactory[MA, Foldable, To]): To = {
      opsFactory(fa)
    }

  }

}
