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

import scala.annotation.compileTimeOnly
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
          case q"""$fa.${TermName("filter" | "withFilter")}(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.core.Preprocessor.Internal.traverseOps(${
              transform(fa)
            }).filter(${
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
        c.macroApplication match {
          case q"$method[$m, $a]($ma)($tc).filter($f)($monadPlus)" =>
            q"_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.filter[$m, $a]($ma, $tc, $monadPlus, $f)"
          case q"$method[$maType]($ma)($unapply).filter($f)($monadPlus)" =>
            val unapplyName = TermName(c.freshName("unapply"))
            q"""{
              val $unapplyName = $unapply
              _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.filter[$unapplyName.M, $unapplyName.A]($ma, $unapplyName.TC, $monadPlus, $f)
            }"""
        }
      }

      def map(f: Tree): Tree = {
        c.macroApplication match {
          case q"$method[$m, $a]($ma)($tc).map[$b]($f)" =>
            q"_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.map[$m, $a, $b]($ma, $tc, $f)"
          case q"$method[$maType]($ma)($unapply).map[$b]($f)" =>
            val unapplyName = TermName(c.freshName("unapply"))
            q"""{
              val $unapplyName = $unapply
              _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.map[$unapplyName.M, $unapplyName.A, $b]($ma, $unapplyName.TC, $f)
            }"""
        }
      }

      def flatMap(f: Tree)(bind: Tree): Tree = {
        c.macroApplication match {
          case q"$method[$m, $a]($ma)($tc).flatMap[$b]($f)($bind)" =>
            q"_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.flatMap[$m, $a, $b]($ma, $tc, $bind, $f)"
          case q"$method[$maType]($ma)($unapply).flatMap[$b]($f)" =>
            val unapplyName = TermName(c.freshName("unapply"))
            q"""{
              val $unapplyName = $unapply
              _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.flatMap[$unapplyName.M, $unapplyName.A, $b]($ma, $unapplyName.TC, $bind, $f)
            }"""
        }
      }

      def foreach(f: Tree): Tree = {
        c.macroApplication match {
          case q"$method[$m, $a]($ma)($tc).foreach[$u]($f)" =>
            q"_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.foreach[$m, $a, $u]($ma, $tc, $f)"
          case q"$method[$maType]($ma)($unapply).foreach[$u]($f)" =>
            val unapplyName = TermName(c.freshName("unapply"))
            q"""{
              val $unapplyName = $unapply
              _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.foreach[$unapplyName.M, $unapplyName.A, $u]($ma, $unapplyName.TC, $f)
            }"""
        }
      }
    }

    sealed trait FakeTraverseOps[F[_], A] {

      def filter(f: A => Boolean)(implicit monadPlus: MonadPlus[F]): F[A] = macro MacroBundle.filter

      def map[B](f: A => B): F[B] = macro MacroBundle.map

      def flatMap[B](f: A => F[B])(implicit bind: Bind[F]): F[B] = macro MacroBundle.flatMap

    }

    sealed trait FakeFoldableOps[F[_], A] {

      def foreach[U](f: A => U): Unit = macro MacroBundle.foreach

    }

    @compileTimeOnly("comprehension must be inside a SDE block.")
    def traverseOps[FA](fa: FA)(implicit unapply: Unapply[Traverse, FA]): FakeTraverseOps[unapply.M, unapply.A] = ???

    @compileTimeOnly("comprehension must be inside a SDE block.")
    def traverseOps[F[_], A](fa: F[A])(implicit traverse: Traverse[F]): FakeTraverseOps[F, A] = ???

    @compileTimeOnly("comprehension must be inside a SDE block.")
    def foldableOps[FA](fa: FA)(implicit unapply: Unapply[Foldable, FA]): FakeFoldableOps[unapply.M, unapply.A] = ???

    @compileTimeOnly("comprehension must be inside a SDE block.")
    def foldableOps[F[_], A](fa: F[A])(implicit foldable: Foldable[F]): FakeFoldableOps[F, A] = ???

  }

}
