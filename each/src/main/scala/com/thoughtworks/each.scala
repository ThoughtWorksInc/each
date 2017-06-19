package com.thoughtworks

import scala.annotation.{StaticAnnotation, TypeConstraint, tailrec}
import scala.collection.immutable.Queue
import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.tools.nsc.typechecker.ContextMode
import scala.tools.nsc.{Global, Mode}

/**
  * @author 杨博 (Yang Bo)
  */
object each {

  /** Replace the annottee to `replacement`.
    *
    * @note Used internally only.
    */
  final class replace(replacement: Any) extends StaticAnnotation with TypeConstraint

  private[each] final case class MonadicTree[G <: Global](tree: G#Tree)

  private[each] def setupAnalyzerPlugin(global: Global): Unit = {

    final class EachAnalyzerPlugin extends global.analyzer.AnalyzerPlugin {

      /** Returns `true` if `that` is a [[EachAnalyzerPlugin]], `false` otherwise.
        *
        * @note The overridding of [[equals]] ensures only one instance of [[EachAnalyzerPlugin]] is registered,
        *       even if [[setupAnalyzerPlugin]] is called more than once.
        */
      override def equals(that: scala.Any): Boolean = {
        that.isInstanceOf[EachAnalyzerPlugin]
      }

      import global._
      import analyzer.Typer

      private val replaceSymbol = symbolOf[replace]

      private var active = true
      private def deact[A](run: => A): A = {
        synchronized {
          active = false
          try {
            run
          } finally {
            active = true
          }
        }
      }

      override def isActive(): Boolean = {
        active && phase.id < currentRun.picklerPhase.id
      }

      override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
        tree match {
          case applyTree @ q"$method[..$typeParameters](...$parameterLists)" =>
            def loop(parameterLists: List[List[Tree]],
                     monadicTrees: Queue[Tree],
                     argumentVals: Queue[ValDef],
                     transformedParameterLists: Queue[Queue[Tree]]): Type = {
              parameterLists match {
                case Nil =>
                  if (monadicTrees.isEmpty) {
                    tpe
                  } else {
                    val f = q"(..$argumentVals) => $method[..$typeParameters](...$transformedParameterLists)"

                    deact {
                      typer.context.withMode(ContextMode.ReTyping) {
                        typer.silent(_.typed(q"(..$monadicTrees).flatMap($f)", Mode.EXPRmode, applyTree.tpe))
                      }
                    }.fold {
                      tree.updateAttachment(MonadicTree(deact {
                        typer.context.withMode(ContextMode.ReTyping) {
                          typer.typed(q"(..$monadicTrees).map($f)", Mode.EXPRmode)
                        }
                      }))
                      tpe
                    } { flatMapped =>
                      tpe.withAnnotations(List(Annotation(deact {
                        typer.context.withMode(ContextMode.ReTyping) {
                          typer.typed(q"new _root_.com.thoughtworks.each.replace($flatMapped)", Mode.EXPRmode)
                        }
                      })))
                    }
                  }
                case headParameterList :: restParameterLists =>
                  @tailrec
                  def innerLoop(parameters: List[Tree],
                                monadicTrees: Queue[Tree],
                                argumentVals: Queue[ValDef],
                                transformedParameters: Queue[Tree]): (Queue[Tree], Queue[ValDef], Queue[Tree]) = {
                    parameters match {
                      case Nil =>
                        (monadicTrees, argumentVals, transformedParameters)
                      case parameter :: restParameters =>
                        parameter.attachments.get[MonadicTree[global.type]] match {
                          case None =>
                            innerLoop(restParameters,
                                      monadicTrees,
                                      argumentVals,
                                      transformedParameters.enqueue(parameter))
                          case Some(MonadicTree(monadicTree)) =>
                            val name = freshTermName()(currentFreshNameCreator)
                            innerLoop(
                              restParameters,
                              monadicTrees.enqueue(monadicTree),
                              argumentVals.enqueue(q"val $name: ${TypeTree(parameter.tpe)} = $EmptyTree"),
                              transformedParameters.enqueue(q"$name")
                            )
                        }
                    }
                  }
                  val (monadicTreesNext, argumentValsNext, transformedParameters) =
                    innerLoop(headParameterList, monadicTrees, argumentVals, Queue.empty)

                  loop(restParameterLists,
                       monadicTreesNext,
                       argumentValsNext,
                       transformedParameterLists.enqueue(transformedParameters))

              }
            }
            loop(parameterLists, Queue.empty, Queue.empty, Queue.empty)

          case _ =>
            tpe
        }

      }

      override def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Boolean = {
        tree.tpe.annotations.exists(_.matches(replaceSymbol))
      }

      override def adaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Tree = {
        tree.tpe.annotations.find(_.matches(replaceSymbol)).flatMap(_.argAtIndex(0)).getOrElse(tree)
      }

    }

    {
      import global.analyzer._
      addAnalyzerPlugin(new EachAnalyzerPlugin)
    }
  }

  private[each] final class Macros(val c: whitebox.Context) {
    import c.universe._

    setupAnalyzerPlugin(c.universe.asInstanceOf[Global])

    def ! : Tree = {
      val q"$bangOps[$f, $a]($fa)" = c.prefix.tree
      val result =
        q"""(throw new _root_.java.lang.AssertionError("Magic call to `!` method should be translated to map/flatMap at compile time")): $a"""
      val global = c.universe.asInstanceOf[Global]
      result.asInstanceOf[global.Tree].updateAttachment(MonadicTree(fa.asInstanceOf[global.Tree]))
      result
    }

    def apply[A: WeakTypeTag](fa: Tree): Tree = {
      val a = weakTypeOf[A]
      val result =
        q"""(throw new _root_.java.lang.AssertionError("Magic call to `each` function should be translated to map/flatMap at compile time")): $a"""
      val global = c.universe.asInstanceOf[Global]
      result.asInstanceOf[global.Tree].updateAttachment(MonadicTree(fa.asInstanceOf[global.Tree]))
      result
    }
  }

  /** Returns each element in `fa`.
    *
    * @example Increasing by 100 to each element in `source`
    *
    *          {{{
    *          val source = Seq(1, 2, 3)
    *          val result = Seq(100 + each(source))
    *          result should be(Seq(101, 102, 103))
    *          }}}
    */
  def apply[F[_], A](fa: F[A]): A = macro Macros.apply[A]

  /** The container of an implicit view to enable the magic method [[BangOps !]]
    *
    * Usage:
    *
    * {{{
    * import com.thoughtworks.each.implicits._
    * }}}
    */
  object implicits {

    implicit class BangOps[F[_], A](fa: F[A]) {

      /** Returns each element in `fa`.
        *
        * This method is an alias of [[each.apply]]
        *
        * @example Increasing by 100 to each element in `source`
        *
        *          {{{
        *          val source = Seq(1, 2, 3)
        *          val result = Seq(100 + source.!)
        *          result should be(Seq(101, 102, 103))
        *          }}}
        */
      def ! : A = macro Macros.!
    }
  }

}
