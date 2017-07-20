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

  private[each] sealed trait MonadicAttachment[G <: Global]
  private[each] object MonadicAttachment {

    private[each] final case class Id[G <: Global](upstream: G#Tree) extends MonadicAttachment[G]
    private[each] final case class Map[G <: Global](upstream: G#Tree, pattern: G#Tree, body: G#Tree)
        extends MonadicAttachment[G]
  }

  private[each] def setupAnalyzerPlugin(global: Global): Unit = {

    final class EachAnalyzerPlugin extends global.analyzer.AnalyzerPlugin {

      /** Returns `true` if `that` is a [[EachAnalyzerPlugin]], `false` otherwise.
        *
        * @note The overridding of [[equals]] ensures only one instance of [[EachAnalyzerPlugin]]
        *       (no matter which ClassLoader the instance belongs to)
        *       is registered,
        *       even if [[setupAnalyzerPlugin]] is called more than once.
        */
      override def equals(that: scala.Any): Boolean = {
        that.getClass.getName == classOf[EachAnalyzerPlugin].getName
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

//      private def attachMonadic(tpe: Type, typer: Typer, tree: Tree, monadicTree: MonadicAttachment[global.type]): Type = {
//        monadicTree match {
//          case MonadicAttachment.Id(upstream) =>
//            ???
//          case MonadicAttachment.Map(upstream, pattern, body) =>
//            ???
//        }
//      }
//
//      private def |@|(fas: Seq[Tree]):Tree = {
//        ???
//      }
//      private def zipPattern(fas: Seq[Tree]):Tree = {
//        ???
//      }

      private def tryFlatMap(typer: Typer,
                             upstream: Tree,
                             pattern: Tree,
                             body: Tree): Either[MonadicAttachment.Map[global.type], Tree] = {
        deact {
          typer.context.withMode(ContextMode.ReTyping) {
            typer.silent(
              _.typed(
                q"""
                ${duplicateAndKeepPositions(upstream)}.flatMap {
                  case ${duplicateAndKeepPositions(pattern)} =>
                    ${duplicateAndKeepPositions(body)}
                }
                """,
                Mode.EXPRmode
              )
            )
          }
        }.fold[Either[MonadicAttachment.Map[global.type], Tree]] {
          Left(MonadicAttachment.Map(upstream, pattern, body))
        } { typed =>
          Right(typed)
        }
      }
      override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {

        def replaceLater(flatten: Tree) = {
          tpe.withAnnotations(List(Annotation(deact {
            typer.context.withMode(ContextMode.ReTyping) {
              typer.typed(q"new _root_.com.thoughtworks.each.replace($flatten)", Mode.EXPRmode)
            }
          })))
        }

        if (mode.inExprMode) {
          tree.attachments.get[MonadicAttachment[global.type]] match {
            case Some(MonadicAttachment.Id(upstream)) =>
              deact {
                typer.context.withMode(ContextMode.ReTyping) {
                  typer.silent(_.typed(q"${duplicateAndKeepPositions(upstream)}.flatten", Mode.EXPRmode))
                }
              }.fold(tpe) { flatten =>
                tree.removeAttachment[MonadicAttachment[global.type]]
                replaceLater(flatten)
              }
            case _ =>
              def makeAnnotationOrAttachment(upstream: Tree, pattern: Tree, body: Tree) = {
                tryFlatMap(
                  typer,
                  upstream,
                  pattern,
                  body
                ) match {
                  case Left(attachment) =>
                    tree.updateAttachment[MonadicAttachment[global.type]](attachment)
                    tpe
                  case Right(flatMapped) =>
                    replaceLater(flatMapped)
                }
              }

              def zippedMap(fas: Queue[Tree], patterns: Queue[Tree], transformedMethod: Tree): Type = {
                if (fas.isEmpty) {
                  tpe
                } else {
                  val zipTree = fas.reduceLeft[Tree] { (l, r) =>
                    q"$l |@| $r"
                  }
                  val pattern = patterns.reduceLeft[Tree] { (l, r) =>
                    pq"($l, $r)"
                  }
                  makeAnnotationOrAttachment(zipTree, pattern, transformedMethod)
                }
              }
              tree match {
                // TODO: check if flatten method available for MonadicAttachment.Id
                case q"$prefix.$field" =>
                  prefix.attachments.get[MonadicAttachment[global.type]] match {
                    case None =>
                      tpe
                    case Some(MonadicAttachment.Map(upstream, pattern, body)) =>
                      makeAnnotationOrAttachment(upstream, pattern, q"$body.$field")
                    case Some(MonadicAttachment.Id(upstream)) =>
                      val prefixName = currentUnit.freshTermName("prefix$")
                      makeAnnotationOrAttachment(upstream, pq"$prefixName", q"$prefixName.$field")
                  }
                case applyTree @ q"$method[..$typeParameters](...$parameterLists)" =>
                  def loop(parameterLists: List[List[Tree]],
                           fas: Queue[Tree],
                           patterns: Queue[Tree],
                           transformedParameterLists: Queue[Queue[Tree]]): Type = {
                    parameterLists match {
                      case Nil =>
                        method.attachments.get[MonadicAttachment[global.type]] match {
                          case None =>
                            zippedMap(fas, patterns, q"$method[..$typeParameters](...$transformedParameterLists)")
                          case Some(MonadicAttachment.Map(upstream, pattern, body)) =>
//                            zippedMap(fas.enqueue(upstream),
//                                      patterns.enqueue(pattern),
//                                      q"$body[..$typeParameters](...$transformedParameterLists)")

                            ??? // FIXME: generate map expression
                          case Some(MonadicAttachment.Id(upstream)) =>
                            val methodName = currentUnit.freshTermName("method$")
                            zippedMap(
                              upstream +: fas,
                              pq"$methodName: ${TypeTree(tpe)}" +: patterns,
                              q"$methodName[..$typeParameters](...$transformedParameterLists)"
                            )

                          //                            insertMonadicTree(
                          //                              (q"$prefixName.$methodName"),
                          //                              monadicPrefix +: monadicTrees,
                          //                              pq"$prefixName: ${TypeTree(prefix.tpe)}" +: parameterPatterns,
                          //                              TypeTree(prefix.tpe) +: parameterTypes,
                          //                              transformedParameterLists
                          //                            )
                        }
//                        method match {
//                          case q"$prefix.$methodName" =>
//                            prefix.attachments.get[MonadicAttachment[global.type]] match {
//                              case None =>
//                                attachFlatMap(fas,
//                                              patterns,
//                                              q"$method[..$typeParameters](...$transformedParameterLists)")
////                            insertMonadicTree(method,
////                                              monadicTrees,
////                                              parameterPatterns,
////                                              parameterTypes,
////                                              transformedParameterLists)
//                              case Some(
//                                  MonadicAttachment
//                                    .Map(prefixUpstream, prefixParameterPattern, transformedPrefix)) =>
//                                ???
//                              case Some(MonadicAttachment.Id(prefixUpstream)) =>
//                                attachFlatMap(
//                                  prefixUpstream +: fas,
//                                  pq"$prefixName: ${TypeTree(prefix.tpe)}" +: patterns,
//                                  q"$prefixName.$methodName[..$typeParameters](...$transformedParameterLists)"
//                                )
//                                ???
////                            val prefixName = currentUnit.freshTermName("prefix$")
////                            insertMonadicTree(
////                              (q"$prefixName.$methodName"),
////                              monadicPrefix +: monadicTrees,
////                              pq"$prefixName: ${TypeTree(prefix.tpe)}" +: parameterPatterns,
////                              TypeTree(prefix.tpe) +: parameterTypes,
////                              transformedParameterLists
////                            )
//                            }
//                          case _ =>
//                            attachFlatMap(fas, patterns, q"$method[..$typeParameters](...$transformedParameterLists)")
////                        insertMonadicTree(method,
////                                          monadicTrees,
////                                          parameterPatterns,
////                                          parameterTypes,
////                                          transformedParameterLists)
//                        }

                      case headParameterList :: restParameterLists =>
                        @tailrec
                        def innerLoop(parameters: List[Tree],
                                      upstreams: Queue[Tree],
                                      patterns: Queue[Tree],
                                      transformedParameters: Queue[Tree]): (Queue[Tree], Queue[Tree], Queue[Tree]) = {
                          assert(upstreams.length == patterns.length)
                          parameters match {
                            case Nil =>
                              (upstreams, patterns, transformedParameters)
                            case parameter :: restParameters =>
                              parameter.attachments.get[MonadicAttachment[global.type]] match {
                                case None =>
                                  innerLoop(restParameters,
                                            upstreams,
                                            patterns,
                                            transformedParameters.enqueue(parameter))
                                case Some(MonadicAttachment.Map(upstream, pattern, body)) =>
                                  innerLoop(
                                    restParameters,
                                    upstreams.enqueue(upstream),
                                    patterns.enqueue(pattern),
                                    transformedParameters.enqueue(body)
                                  )
                                case Some(MonadicAttachment.Id(upstream)) =>
                                  val name = currentUnit.freshTermName()
                                  innerLoop(
                                    restParameters,
                                    upstreams.enqueue(upstream),
                                    patterns.enqueue(pq"$name: ${TypeTree(parameter.tpe)}"),
                                    transformedParameters.enqueue(q"$name")
                                  )
                              }
                          }
                        }
                        val (monadicTreesNext, parameterPatternsNext, transformedParameters) =
                          innerLoop(headParameterList, fas, patterns, Queue.empty)
                        loop(restParameterLists,
                             monadicTreesNext,
                             parameterPatternsNext,
                             transformedParameterLists.enqueue(transformedParameters))

                    }
                  }
                  loop(parameterLists, Queue.empty, Queue.empty, Queue.empty)

                case _ =>
                  tpe
              }
          }
        } else {
          tpe
        }
      }

      override def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Boolean = {
        tree.tpe.annotations.exists(_.matches(replaceSymbol))
      }

      override def adaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Tree = {
        val result = tree.tpe.annotations.find(_.matches(replaceSymbol)).flatMap(_.argAtIndex(0)).getOrElse(tree)
        println("replace " + tree + " to " + result)
        result
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
      result
        .asInstanceOf[global.Tree]
        .updateAttachment[MonadicAttachment[global.type]](MonadicAttachment.Id(fa.asInstanceOf[global.Tree]))
      result
    }

    def apply[A: WeakTypeTag](fa: Tree): Tree = {
      val a = weakTypeOf[A]
      val result =
        q"""(throw new _root_.java.lang.AssertionError("Magic call to `each` function should be translated to map/flatMap at compile time")): $a"""
      val global = c.universe.asInstanceOf[Global]
      result
        .asInstanceOf[global.Tree]
        .updateAttachment[MonadicAttachment[global.type]](MonadicAttachment.Id(fa.asInstanceOf[global.Tree]))
      result
    }
  }

  /** Returns each element in `fa`.
    *
    * @example xx
    *          {{{
    *          val source = Seq(1, 2, 3)
    *          val result = Seq(each(source) * each(source))
    *          result should be(Seq(1, 4, 9))
    *          }}}
    *
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
