package com.thoughtworks

import com.thoughtworks.each.Monadic

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.higherKinds
import com.thoughtworks.each.Monadic._
import com.thoughtworks.sde.core.MonadicTransformer
import com.thoughtworks.sde.core.MonadicTransformer._

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.collection.LinearSeqOptimized
import scala.collection.generic.{GenericTraversableTemplate, SeqFactory}
import scala.reflect.macros.{blackbox, whitebox}
import scalaz.Free.Source
import scalaz.{-\/, Applicative, Apply, Free, Traverse, Traverse1, \/-}
import scalaz.std.list._
import scalaz.std.tuple._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
final class generator[Element] extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro generator.transformAnnotation

}

object generator {

  object Implicits {

    import scala.language.implicitConversions

    implicit final class GenOps[A, B](generator: Source[A, B]) {
      @compileTimeOnly("`gen` must be inside an `generator` block.")
      def gen: B = ???
    }

    implicit final class AnyGenOps[A](a: A) {
      @compileTimeOnly("`gen` must be inside an `generator` block.")
      def gen: Unit = ???
    }

    implicit def sourceToGenerator[A](freeSource: Source[A, Unit]): Generator[A] = {
      Generator.sourceToGenerator(freeSource)
    }
  }

  def monadic[A, B](element: A, typedBody: B): Source[A, B] = macro transformMonadic

  def transformMonadic(c: blackbox.Context)(element: c.Tree, typedBody: c.Tree): c.Tree = {
    import c.universe._

    val genOpsSymbol = {
      typeOf[_root_.com.thoughtworks.generator.Implicits.type].member(TermName("GenOps"))
    }

    val anyGenOpsSymbol = {
      typeOf[_root_.com.thoughtworks.generator.Implicits.type].member(TermName("AnyGenOps"))
    }

    val monadicComprehensionSymbol = {
      typeOf[_root_.com.thoughtworks.sde.MonadicComprehension.type].termSymbol
    }

    val monadicTransformer = new MonadicTransformer[c.universe.type](c.universe, UnsupportedExceptionHandlingMode) {
      override protected val instructionExtractor: PartialFunction[Tree, Instruction] = {
        case select@q"""$genOps[$a, $b]($fa).gen""" if genOps.symbol == genOpsSymbol =>
          Each(fa)
        case select@q"""$anyGenOps[$a]($fa).gen""" if anyGenOps.symbol == anyGenOpsSymbol =>
          Each(q"""_root_.scalaz.Free.produce[$a]($fa)""")
        case tree@q"""$monadicComprehension.traverseOps[$fType, $aType]($fa)($traverse).flatMap[$b](${f: Function})($bind)""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          FlatMap(
            q"""_root_.scalaz.syntax.traverse.ToTraverseOps[$fType, $aType]($fa)($traverse)""",
            b,
            f,
            bind,
            appliedType(fType.tpe, b.tpe),
            locally
          )
        case tree@q"""$monadicComprehension.traverseOps[$faType]($fa)($upapply).flatMap[$b](${f: Function})($bind)""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          FlatMap(
            q"""_root_.scalaz.syntax.traverse.ToTraverseOpsUnapply[$faType]($fa)($upapply)""",
            b,
            f,
            bind,
            internal.typeRef(upapply.tpe, upapply.tpe.member(TypeName("M")), List(b.tpe)),
            locally
          )
        case q"""$monadicComprehension.traverseOps[$fType, $aType]($fa)($traverse).map[$b](${f: Function})""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          Map(
            q"""_root_.scalaz.syntax.traverse.ToTraverseOps[$fType, $aType]($fa)($traverse)""",
            f,
            appliedType(fType.tpe, b.tpe),
            locally
          )
        case q"""$monadicComprehension.traverseOps[$faType]($fa)($upapply).map[$b](${f: Function})""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          Map(
            q"""_root_.scalaz.syntax.traverse.ToTraverseOpsUnapply[$faType]($fa)($upapply)""",
            f,
            internal.typeRef(upapply.tpe, upapply.tpe.member(TypeName("M")), List(b.tpe)),
            locally
          )
        case tree@q"""$monadicComprehension.foldableOps[$fType, $aType]($fa)($foldable).foreach[$u](${f: Function})""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          Foreach(
            q"""_root_.scalaz.syntax.foldable.ToFoldableOps[$fType, $aType]($fa)($foldable)""",
            f
          )
        case tree@q"""$monadicComprehension.foldableOps[$faType]($fa)($unapply).foreach[$u](${f: Function})""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          Foreach(
            q"""_root_.scalaz.syntax.foldable.ToFoldableOpsUnapply[$faType]($fa)($unapply)""",
            f
          )
        case tree@q"""$monadicComprehension.traverseOps[$fType, $aType]($fa)($traverse).filter(${f: Function})($monadPlus)""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          Filter(
            q"""_root_.scalaz.syntax.traverse.ToTraverseOps[$fType, $aType]($fa)($traverse)""",
            f,
            monadPlus,
            appliedType(fType.tpe, aType.tpe),
            locally
          )
        case tree@q"""$monadicComprehension.traverseOps[$faType]($fa)($unapply).filter(${f: Function})($monadPlus)""" if monadicComprehension.symbol == monadicComprehensionSymbol =>
          Filter(
            q"""_root_.scalaz.syntax.traverse.ToTraverseOpsUnapply[$faType]($fa)($unapply)""",
            f,
            monadPlus,
            faType.tpe,
            locally
          )
      }

      override protected def freshName(name: String): String = c.freshName(name)

      // See https://issues.scala-lang.org/browse/SI-5712
      override protected def fTree: Tree =
        tq"""({type T[A] = _root_.scalaz.Free.Source[${element.tpe}, A]})#T"""
    }

    val monadicTree = monadicTransformer.transform(typedBody, q"""_root_.scalaz.Free.sourceMonad[${element.tpe}]""")
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
            q"""_root_.com.thoughtworks.sde.MonadicComprehension.traverseOps(${
              transform(fa)
            }).filter(${
              transform(f)
            })"""
          case q"""$fa.map(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.MonadicComprehension.traverseOps(${
              transform(fa)
            }).map(${
              transform(f)
            })"""
          case q"""$fa.foreach(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.MonadicComprehension.foldableOps(${
              transform(fa)
            }).foreach(${
              transform(f)
            })"""
          case q"""$fa.flatMap(${f: Function})""" =>
            q"""_root_.com.thoughtworks.sde.MonadicComprehension.traverseOps(${
              transform(fa)
            }).flatMap(${
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

  private def transformBody(c: whitebox.Context)(elementType: c.Tree, body: c.Tree): c.Tree = {
    import c.universe._
    val freeSourceName = TermName(c.freshName("freeSource"))
    q"""{
      import _root_.com.thoughtworks.generator.Implicits._
      val $freeSourceName = _root_.com.thoughtworks.generator.monadic(_root_.scala.Predef.??? : $elementType, ${virtualizeComprehension(c)(body)})
      $freeSourceName
    }"""
  }

  def transformAnnotation(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {

    import c.universe._

    val q"""new $_[$elementTypeTree]().$_($_)""" = c.macroApplication

    val result = annottees match {
      case Seq(annottee@DefDef(mods, name, tparams, vparamss, tpt, rhs)) =>
        atPos(annottee.pos) {
          DefDef(mods, name, tparams, vparamss, tpt, transformBody(c)(elementTypeTree, rhs))
        }
      case Seq(annottee@ValDef(mods, name, tpt, rhs)) =>
        atPos(annottee.pos) {
          ValDef(mods, name, tpt, transformBody(c)(elementTypeTree, rhs))
        }
      case _ =>
        c.error(c.enclosingPosition, "Expect def or val")
        annottees.head
    }

    //    c.info(c.enclosingPosition, show(result), true)

    result
  }

  object Generator extends SeqFactory[Generator] {

    override final def newBuilder[Element] = {
      List.newBuilder[Element].mapResult { upstream =>

        type BuildingSource[A] = Source[Element, A]

        def buildingSource: BuildingSource[_] = Monadic.monadic[BuildingSource] {
          for (element <- upstream.monadicLoop) {
            (Free.produce(element): BuildingSource[Unit]).each
          }
        }
        sourceToGenerator(buildingSource)
      }
    }

    implicit def sourceToGenerator[A](freeSource: Source[_ <: A, _]): Generator[A] = {
      freeSource.resume match {
        case -\/((head, tailSource)) => NonEmpty(head, tailSource)
        case _ => Empty
      }
    }

    override final def empty[A] = Empty

    private[Generator] final case object Empty extends Generator[Nothing] {

      override final def isEmpty: Boolean = true

      override final def head: Nothing = {
        throw new NoSuchElementException("head of empty list")
      }

      override final def tail: Nothing = {
        throw new UnsupportedOperationException("tail of empty list")
      }

    }

    import scala.language.existentials
    private[Generator] final case class NonEmpty[+A](override val head: A, tailSource: Source[_ <: A, _]) extends Generator[A] {

      override final def isEmpty: Boolean = false

      override final def tail: Generator[A] = {
        sourceToGenerator(tailSource)
      }

    }

  }

  sealed abstract class Generator[+A]
    extends scala.collection.immutable.Seq[A]
      with scala.collection.immutable.LinearSeq[A]
      with GenericTraversableTemplate[A, Generator]
      with LinearSeqOptimized[A, Generator[A]] {

    override final def companion = Generator

  }

}