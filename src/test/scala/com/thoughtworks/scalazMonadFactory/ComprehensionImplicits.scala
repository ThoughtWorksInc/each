package com.thoughtworks.scalazMonadFactory

import scalaz._

object ComprehensionImplicits {

  implicit def comprehensionApplicative[F[_]]: Applicative[F] = macro ComprehensionApplicative.applyImpl

  implicit def comprehensionBind[F[_]]: Bind[F] = ???

  implicit def comprehensionMonad[F[_]]: Monad[F] = ???

}
