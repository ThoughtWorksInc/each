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

import com.thoughtworks.sde.core.MonadicTransformer
import com.thoughtworks.sde.core.MonadicTransformer.MonadThrowableMode

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.macros.{blackbox, whitebox}
import scala.language.experimental.macros
import scalaz._
import scalaz.Id.Id
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
final class future extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro future.transformAnnotation

}






// TODO: comprehension (easy)
// TODO: reuseability
// TODO: EitherT, OptionT, ... 似乎可以通过 Instruction 实现
// 如何区分手动指定隐式参数的 comprehension 和没有隐式参数的? 不区分,只要在async块,就认为是monad版的
object future {

  object Implicits {

    import scala.language.implicitConversions

    implicit final class AwaitOps[A](future: Future[A]) {
      @compileTimeOnly("`await` must be inside an `future` block.")
      def await: A = ???
    }

    @compileTimeOnly("`await` must be inside an `future` block.")
    implicit def await[A](future: Future[A]): A = ???

  }

  def monadic[A](typedBody: A): Future[A] = macro transformMonadic

  def transformMonadic(c: blackbox.Context)(typedBody: c.Tree): c.Tree = {
    import c.universe._

    val awaitOpsSymbol = {
      typeOf[_root_.com.thoughtworks.sde.future.Implicits.type].member(TermName("AwaitOps"))
    }

    val prefixAwaitMethodSymbol = {
      typeOf[_root_.com.thoughtworks.sde.future.Implicits.type].member(TermName("await"))
    }

    val monadicComprehensionSymbol = {
      typeOf[_root_.com.thoughtworks.sde.MonadicComprehension.type].termSymbol
    }

    val monadicTransformer = new MonadicTransformer[c.universe.type](c.universe, MonadThrowableMode) {
      override protected val instructionExtractor: PartialFunction[Tree, Instruction] = {
        case select@q"""$awaitOps[$a]($fa).await""" if awaitOps.symbol == awaitOpsSymbol =>
          Each(fa)
        case q"""$await[$a]($fa)""" if await.symbol == prefixAwaitMethodSymbol =>
          Each(fa)
        case tree@q"""$monadicComprehension.flatMap[$fType, $a, $b]($fa)(${f: Function})($traverse, $bind)""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          FlatMap(
            q"""_root_.scalaz.syntax.traverse.ToTraverseOps[$fType, $a]($fa)($traverse)""",
            b,
            f,
            bind,
            appliedType(fType.tpe, b.tpe),
            locally
          )
        case q"""$monadicComprehension.map[$fType, $a, $b]($fa)(${f: Function})($traverse)""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          Map(
            q"""_root_.scalaz.syntax.traverse.ToTraverseOps[$fType, $a]($fa)($traverse)""",
            f,
            appliedType(fType.tpe, b.tpe),
            locally
          )
        case tree@q"""$monadicComprehension.foreach[$fType, $a, $u]($fa)(${f: Function})($foldable)""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          Foreach(
            q"""_root_.scalaz.syntax.foldable.ToFoldableOps[$fType, $a]($fa)($foldable)""",
            f
          )
        case tree@q"""$monadicComprehension.filter[$fType, $a]($fa)(${f: Function})($traverse, $monadPlus)""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          Filter(
            q"""_root_.scalaz.syntax.traverse.ToTraverseOps[$fType, $a]($fa)($traverse)""",
            f,
            monadPlus,
            appliedType(fType.tpe, a.tpe),
            locally
          )
      }

      override protected def freshName(name: String): String = c.freshName(name)

      // See https://issues.scala-lang.org/browse/SI-5712
      override protected def fTree: Tree =
        tq"""_root_.scala.concurrent.Future"""
    }

    val monadicTree = monadicTransformer.transform(typedBody, q"""_root_.scalaz.std.scalaFuture.futureInstance""")
    //    c.info(c.enclosingPosition, show(monadicTree), true)
    c.untypecheck(monadicTree)
  }

  private def virtualizeComprehension(c: whitebox.Context)(tree: c.Tree): c.Tree = {
    import c.universe._

//    c.info(c.enclosingPosition, show(tree), true)
//    c.info(c.enclosingPosition, showRaw(tree), true)

    val transformer = new Transformer {
      override def transform(tree: Tree): Tree = {
        tree match {
          case q"""$fa.filter(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.MonadicComprehension.filter(${
              transform(fa)
            })(${
              transform(f)
            })"""
          case q"""$fa.withFilter(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.MonadicComprehension.filter(${
              transform(fa)
            })(${
              transform(f)
            })"""
          case q"""$fa.map(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.MonadicComprehension.map(${
              transform(fa)
            })(${
              transform(f)
            })"""
          case q"""$fa.foreach(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.MonadicComprehension.foreach(${
              transform(fa)
            })(${
              transform(f)
            })"""
          case q"""$fa.flatMap(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.MonadicComprehension.flatMap(${
              transform(fa)
            })(${
              transform(f)
            })"""
          case _ =>
            super.transform(tree)
        }
      }
    }
    atPos(tree.pos) {
      transformer.transform(tree)
    }
  }

  private def transformBody(c: whitebox.Context)(body: c.Tree): c.Tree = {
    import c.universe._
    q"""{
      import _root_.com.thoughtworks.sde.future.Implicits._
      _root_.com.thoughtworks.sde.future.monadic(${virtualizeComprehension(c)(body)})
    }"""
  }

  def transformAnnotation(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {

    import c.universe._
    annottees match {
      case Seq(annottee@DefDef(mods, name, tparams, vparamss, tpt, rhs)) =>
        atPos(annottee.pos) {
          DefDef(mods, name, tparams, vparamss, tpt, transformBody(c)(rhs))
        }
      case Seq(annottee@ValDef(mods, name, tpt, rhs)) =>
        atPos(annottee.pos) {
          ValDef(mods, name, tpt, transformBody(c)(rhs))
        }
      case _ =>
        c.error(c.enclosingPosition, "Expect def or val")
        annottees.head
    }
  }

}