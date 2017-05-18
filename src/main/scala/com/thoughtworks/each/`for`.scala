package com.thoughtworks.each

import scala.annotation.{StaticAnnotation, compileTimeOnly}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object `for` {

  // 和3.x相比，优点在于无需指定Monad的类型，而可以直接根据From的类型来生成
  def apply()

  // FIXME: 应该直接用flatMap
  trait FlatMap[From, Elem, To] {

    def apply(from: From, f: Elem => To): To

  }

  trait Map[From, A, B] {

    type To

    def bang: Bang.Aux[To, B]

    def apply(from: From, f: A => B): To

  }

  object Bang {
    type Aux[From, Elem0] = Bang[From] {
      type Elem = Elem0
    }
  }
  trait Bang[From] {
    type Elem
  }

  object implicits {
    implicit final class BangOps[From](from: From) {

      @compileTimeOnly(message = "! syntax must only be used in `for` block")
      def !(implicit bang: Bang[From]): bang.Elem = ???

    }
  }
}
