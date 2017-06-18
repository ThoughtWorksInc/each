package com.thoughtworks

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
  * @example xxx
  *          {{{
  *          import each.implicits._
  *          val i = 0
  *          val j = 5
  *          val x = Seq({
  *            import java.io.InputStream
  *            Seq(i, j).!
  *          })
  *          println("Hello, World")
  *          }}}
  * @author 杨博 (Yang Bo)
  */
object each {
  final class monadic(fa: Any) extends StaticAnnotation

  @compileTimeOnly(
    message =
      """This function is only available if `addCompilerPlugin("com.thoughtworks.each" %% "eachplugin" % "latest.release")` setting exsits""")
  def apply[A](fa: Any): A = ???

  private[each] final class Macros(val c: whitebox.Context) {

    import c.universe._
    def ! : Tree = {
      val q"$bangOps[$f, $a]($fa)" = c.prefix.tree
      q"_root_.com.thoughtworks.each.apply[$a]($fa)"
    }
  }

  object implicits {

    implicit class BangOps[F[_], A](fa: F[A]) {
      def ! : A = macro Macros.!
    }
  }

}
