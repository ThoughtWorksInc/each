package com.thoughtworks.sde

import scala.annotation.compileTimeOnly
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait NestedTransformer[M[_]] {

  @compileTimeOnly("Nested transformer must be inside a monadic block.")
  def apply[A](a: A): M[A] = ???

}
