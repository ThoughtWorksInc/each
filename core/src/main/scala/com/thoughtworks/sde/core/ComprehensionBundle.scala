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

import macrocompat.bundle

import scala.annotation.compileTimeOnly
import scalaz.{Bind, Foldable, MonadPlus, Traverse, Unapply}
import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@bundle
class ComprehensionBundle(val c: whitebox.Context) {

  import c.universe._

  class Virtualizer extends Transformer {
    override def transform(tree: Tree): Tree = {
      tree match {
        case q"""$fa.filter(${f: Function})""" =>
          q"""_root_.com.thoughtworks.sde.ComprehensionBundle.Internal.filter(${
            transform(fa)
          })(${
            transform(f)
          })"""
        case q"""$fa.withFilter(${f: Function})""" =>
          q"""_root_.com.thoughtworks.sde.ComprehensionBundle.Internal.traverseOps(${
            transform(fa)
          }).filter(${
            transform(f)
          })"""
        case q"""$fa.map(${f: Function})""" =>
          q"""_root_.com.thoughtworks.sde.ComprehensionBundle.Internal.traverseOps(${
            transform(fa)
          }).map(${
            transform(f)
          })"""
        case q"""$fa.foreach(${f: Function})""" =>
          q"""_root_.com.thoughtworks.sde.ComprehensionBundle.Internal.foldableOps(${
            transform(fa)
          }).foreach(${
            transform(f)
          })"""
        case q"""$fa.flatMap(${f: Function})""" =>
          q"""_root_.com.thoughtworks.sde.ComprehensionBundle.Internal.traverseOps(${
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

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object ComprehensionBundle {
  
  object Internal {
  
    @bundle
    private[Internal] final class MacroBundle(val c: whitebox.Context) {
      import c.universe._
      def filter(f: Tree)(monadPlus: Tree): Tree = ???
      def map(f: Tree): Tree = ???
      def flatMap(f: Tree)(bind: Tree): Tree = ???
      def foreach(f: Tree): Tree = ???
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
