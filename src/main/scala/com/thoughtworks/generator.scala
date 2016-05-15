package com.thoughtworks

import com.thoughtworks.each.Monadic._

import scala.collection.LinearSeqOptimized
import scala.collection.generic.{GenericTraversableTemplate, SeqFactory}
import scalaz.{-\/, Free}
import scalaz.std.list._
import scalaz.std.tuple._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class generator {

}

object generator {


  object Generator extends SeqFactory[Generator] {

    override final def newBuilder[Element] = {
      List.newBuilder[Element].mapResult { upstream =>
        type BuildingSource[A] = Free.Source[Element, A]
        val buildingSource = monadic[BuildingSource] {
          for (element <- upstream.monadicLoop) {
            (Free.produce(element): BuildingSource[Unit]).each
          }
        }
        freeSourceToGenerator(buildingSource)
      }
    }

    implicit def freeSourceToGenerator[A](freeSource: Free.Source[A, Unit]) = {
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

    private[Generator] final case class NonEmpty[A](override val head: A, tailSource: Free.Source[A, Unit]) extends Generator[A] {

      override final def isEmpty: Boolean = false

      override final def tail: Generator[A] = {
        freeSourceToGenerator(tailSource)
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