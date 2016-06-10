package com.thoughtworks.sde.comprehensionMonad

import macrocompat.bundle

import scalaz.Monad
import scala.language.higherKinds
import scala.language.experimental.macros

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object ComprehensionMonad {

  /**
    * Returns a monad implemented by `for` comprehension syntax.
    *
    * `F` must support `for` comprehension, containing `map` and `flatMap` methods,
    * and `F`'s companion object must contains an `apply` method to create a `F[A]` instance from any `A`.
    *
    * @tparam F the higher kinded type that the monad works with, e.g. a Scala container `Seq`.
    * @return the monad.
    */
  implicit def comprehensionMonad[F[_]]: Monad[F] = macro MacroBundle.comprehensionMonad

  abstract class AbstractMonad[F0[_]] extends scalaz.Monad[F0] {
    protected type F[A] = F0[A]
  }

  @bundle
  private[ComprehensionMonad] final class MacroBundle(val c: scala.reflect.macros.blackbox.Context) {

    import c.universe._

    def comprehensionMonad: Tree = {
      val q"$method[$f]" = c.macroApplication
      val fSymbol = f.tpe.typeSymbol.asType
      val fCompanion = Ident(f.tpe.typeSymbol.asType.companion)
      q"""
        new _root_.com.thoughtworks.sde.comprehensionMonad.ComprehensionMonad.AbstractMonad[$f] {
          override final def bind[A, B](fa: F[A])(f: A => F[B]) = fa.flatMap(f)
          override final def point[A](a: => A) = $fCompanion.apply(a)
          override final def map[A, B](fa: F[A])(f: A => B) = fa.map(f)
        }
      """
    }
  }

}
