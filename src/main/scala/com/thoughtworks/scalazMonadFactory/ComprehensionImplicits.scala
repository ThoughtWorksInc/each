package com.thoughtworks.scalazMonadFactory

import scalaz._

import scala.language.experimental.macros
import scala.language.higherKinds

object ComprehensionImplicits extends ComprehensionImplicits

trait ComprehensionImplicits {

  implicit def comprehensionApplicative[F[_]]: Applicative[F] = macro ComprehensionApplicative.applyImpl

  implicit def comprehensionBind[F[_]]: Bind[F] = ???

  implicit def comprehensionMonad[F[_]]: Monad[F] = ???

}
