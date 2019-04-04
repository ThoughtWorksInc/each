package com.thoughtworks.sde

import com.thoughtworks.sde.core.{MonadicFactory, Preprocessor}
import macrocompat.bundle

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.collection.generic.{GenericTraversableTemplate, SeqFactory}
import scala.collection.{IndexedSeq, LinearSeq, LinearSeqOptimized}
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.macros.whitebox
import scalaz.Free.Source
import scalaz._
import scalaz.std.list._
import scalaz.std.tuple._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
final class source[Element] extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro source.AnnotationBundle.macroTransform

}

object source {

  @bundle
  private[source] final class AnnotationBundle(context: whitebox.Context) extends Preprocessor(context) {

    import c.universe._

    def macroTransform(annottees: Tree*): Tree = {
      replaceDefBody(annottees, { body =>
        val q"new $annotationClass[$elementType]().macroTransform(..$arguments)" = c.macroApplication
        q"""
          _root_.com.thoughtworks.sde.core.MonadicFactory[
            _root_.scalaz.Monad,
            ({type T[A] = _root_.scalaz.Free.Source[$elementType, A]})#T
          ].apply {
            import _root_.com.thoughtworks.sde.source.AutoImports._
            ${(new ComprehensionTransformer).transform(body)}
          }(_root_.scalaz.Free.sourceMonad[$elementType])
        """
      })
    }

  }

  final def seqToSource[A](elements: Seq[A]): Source[A, Unit] = {
    elements match {
      case indexed: IndexedSeq[A] => indexedSeqToSource(indexed)
      case linear: LinearSeq[A] => linearSeqToSource(linear)
    }
  }

  final def linearSeqToSource[A](elements: LinearSeq[A]): Source[A, Unit] = {
    elements match {
      case LinearSeq(head, tail: LinearSeq[A]) =>
        Free.produce(head).flatMap { _: Unit =>
          linearSeqToSource[A](tail)
        }
      case _ =>
        Free.point[(A, ?), Unit](())
    }
  }

  final def indexedSeqToSource[A](elements: IndexedSeq[A], i: Int = 0): Source[A, Unit] = {
    if (i == elements.length) {
      Free.point[(A, ?), Unit](())
    } else {
      Free.produce(elements(i)).flatMap { _: Unit =>
        indexedSeqToSource(elements, i + 1)
      }
    }
  }

  @bundle
  private[source] final class YieldBundle(val c: whitebox.Context) {

    import c.universe._

    def prefixYieldAll(elements: Tree*): Tree = {
      val q"$yieldAll[$a](..$elements)" = c.macroApplication
      val fName = TypeName(c.freshName("F"))
      q"""
        type $fName[A] = _root_.scalaz.Free.Source[$a, A]
        ..${
        for {
          element <- elements
        } yield
          q"""
            _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[$fName, _root_.scala.Unit](
              _root_.scalaz.Free.produce($element)
            )
          """
      }
      """
    }

    def postfixYieldAll: Tree = {
      val opsName = TermName(c.freshName("ops"))
      val q"$ops.yieldAll" = c.macroApplication
      q"""
        val $opsName = $ops
        _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[$opsName.F, $opsName.A]($opsName.underlying)
      """
    }

    def yieldOne: Tree = {
      val opsName = TermName(c.freshName("ops"))
      val q"$ops.yieldOne" = c.macroApplication
      q"""
        val $opsName = $ops
        _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[$opsName.F, $opsName.A](_root_.scalaz.Free.produce($ops.underlying))
      """
    }

  }

  object AutoImports {

    import scala.language.implicitConversions

    implicit object UnitSourceInstance extends MonadPlus[Source[?, Unit]] {

      private type F[A] = Source[A, Unit]

      override def bind[A, B](fa: F[A])(f: (A) => F[B]): F[B] = {
        fa.resume match {
          case -\/((head, tailSource)) => {
            f(head).flatMap { _: Unit =>
              bind(tailSource)(f)
            }
          }
          case \/-(a) =>
            Free.point[(B, ?), Unit](())
        }
      }

      override def empty[A]: F[A] = {
        Free.point[(A, ?), Unit](())
      }

      override def plus[A](a: F[A], b: => F[A]): F[A] = {
        a.flatMap { _: Unit => b }
      }

      override def point[A](a: => A): F[A] = {
        Free.produce(a)
      }
    }

    // This type class is not available on current scalaz version. See https://github.com/scalaz/scalaz/pull/1160
    implicit def sourceTraverse[A0]: Traverse[Source[?, A0]] = {
      type F[A] = Source[A, A0]
      new Traverse[F] {
        def traverseImpl[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = {
          fa.resume match {
            case -\/((head, tailSource)) => {
              implicitly[Applicative[G]].apply2(f(head), traverseImpl[G, A, B](tailSource)(f)) { (mappedHead: B, mappedTail: F[B]) =>
                Free.produce(mappedHead).flatMap { _: Unit =>
                  mappedTail
                }
              }
            }
            case \/-(a) =>
              implicitly[Applicative[G]].point(Free.point[(B, ?), A0](a))
          }
        }

      }
    }

    /** Enable type classes at type parameter position 0 of Free.Source */
    implicit def unapplyMTMAB0[TC[_[_]], MT[_[_], _], MAB[_, _], A0, A1](implicit TC0: TC[λ[α => MT[MAB[α, ?], A1]]]):
    Unapply[TC, MT[MAB[A0, ?], A1]] {
      type M[X] = MT[MAB[X, ?], A1]
      type A = A0
    } = new Unapply[TC, MT[MAB[A0, ?], A1]] {
      type M[X] = MT[MAB[X, ?], A1]
      type A = A0

      def TC = TC0

      def leibniz = Leibniz.refl
    }

    final def yieldAll[A](elements: A*): Unit = macro YieldBundle.prefixYieldAll

    implicit final class YieldAllOps[Element, B](val underlying: Source[Element, B]) extends AnyVal {
      type A = B
      type F[X] = Source[Element, X]

      def yieldAll: B = macro YieldBundle.postfixYieldAll
    }

    implicit final class YieldOneOps[Element](val underlying: Element) extends AnyVal {
      type A = Unit
      type F[X] = Source[Element, X]

      def yieldOne: Unit = macro YieldBundle.yieldOne
    }

    implicit def sourceToSeq[A](freeSource: Source[A, _]): SourceSeq[A] = {
      SourceSeq.sourceToSeq(freeSource)
    }

  }

  def sourceToSeq[A](freeSource: Source[A, _]): SourceSeq[A] = {
    SourceSeq.sourceToSeq(freeSource)
  }

  def apply[Element] = MonadicFactory.WithTypeClass[Monad, Source[Element, ?]]

  object SourceSeq extends SeqFactory[SourceSeq] {

    override final def newBuilder[Element] = {
      List.newBuilder[Element].mapResult { upstream =>

        type BuildingSource[A] = Source[Element, A]

        def buildingSource: BuildingSource[_] = {
          import scalaz.syntax.traverse._
          upstream.traverse { element =>
            Free.produce(element)
          }
        }
        sourceToSeq(buildingSource)
      }
    }

    implicit def sourceToSeq[A](freeSource: Source[A, _]): SourceSeq[A] = {
      freeSource.resume match {
        case -\/((head, tailSource)) => NonEmpty(head, tailSource)
        case _ => Empty
      }
    }

    override final def empty[A] = Empty

    private[SourceSeq] final case object Empty extends SourceSeq[Nothing] {

      override def isEmpty: Boolean = true

      override def head: Nothing = {
        throw new NoSuchElementException("head of empty list")
      }

      override def tail: Nothing = {
        throw new UnsupportedOperationException("tail of empty list")
      }

    }

    import scala.language.existentials

    private[SourceSeq] final case class NonEmpty[+A](override val head: A, tailSource: Source[_ <: A, _]) extends SourceSeq[A] {

      override def isEmpty: Boolean = false

      override def tail: SourceSeq[A] = {
        sourceToSeq(tailSource)
      }

    }

    override final def apply[A](elements: A*) = {
      sourceToSeq(seqToSource(elements))
    }

  }

  sealed abstract class SourceSeq[+A]
    extends scala.collection.immutable.Seq[A]
      with scala.collection.immutable.LinearSeq[A]
      with GenericTraversableTemplate[A, SourceSeq]
      with LinearSeqOptimized[A, SourceSeq[A]] {

    override final def companion = SourceSeq

  }

}
