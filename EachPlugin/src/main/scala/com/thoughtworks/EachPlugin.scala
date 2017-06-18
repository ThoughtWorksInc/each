package com.thoughtworks

import java.io._
import java.net.URI

import com.thoughtworks.each.monadic

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.reflect.internal.NoPhase
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.AbstractFile
import scala.reflect.macros.compiler.Errors
import scala.tools.nsc
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.tools.nsc.typechecker.{Analyzer, AnalyzerPlugins, Namers}
import scala.tools.nsc.{Global, Mode, Phase}

/**
  * @example xxx
  *          {{{
  *          import each.implicits._
  *          val i = 0
  *          val j = 5
  *          val x = Seq(each[Int](Seq(i, j)))
  *          val y = Seq(each[Int](x))
  *          val z = Seq(3 + x.!)
  *          println("Hello, World")
  *          }}}
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class EachPlugin(override val global: Global) extends Plugin {

  import global._
  final case class MonadicTree(tree: Tree)

  override val name: String = "each"

  override val components: List[PluginComponent] =
    List(
      new PluginComponent with Transform with TypingTransformers {
        override val global: EachPlugin.this.global.type = EachPlugin.this.global

        override val phaseName: String = EachPlugin.this.name
//        override val runsRightAfter: Some[String] = Some("typer")
        override val runsAfter: List[String] = List("typer")

        override def newPhase(prev: nsc.Phase): StdPhase = {
          super.newPhase(prev)
        }

        override protected def newTransformer(unit: CompilationUnit): Transformer = new TypingTransformer(unit) {
          private val eachApplySymbol = symbolOf[each.type].info.member(TermName("apply"))
//          private val monadicSymbol = symbolOf[monadic]
          override def transform(tree: Tree): Tree = {
            super.transform(tree) match {
//              case
              case eachTree @ q"$method[$a]($fa)" if method.symbol == eachApplySymbol =>
                println("update")
                println(eachTree)
                println()
                eachTree.updateAttachment(MonadicTree(fa))
                eachTree
              case applyTree @ q"$method[..$typeParameters](...$parameterLists)" =>
                def loop(parameterLists: List[List[Tree]],
                         monadicTrees: Queue[Tree],
                         argumentVals: Queue[ValDef],
                         transformedParameterLists: Queue[Queue[Tree]]): Tree = {
                  parameterLists match {
                    case Nil =>
                      if (monadicTrees.isEmpty) {
                        applyTree
                      } else {
                        reporter.info(tree.pos, "3333333", true)
                        println(
                          show(q"(..$argumentVals) => $method[..$typeParameters](...$transformedParameterLists)"))
                        val f = localTyper.typed(
                          q"(..$argumentVals) => $method[..$typeParameters](...$transformedParameterLists)",
                          Mode.EXPRmode)

                        localTyper
                          .silent(_.typed(q"(..$monadicTrees).flatMap($f)", Mode.EXPRmode, applyTree.tpe.widen))
                          .orElse { ignoredTypeErrors =>
                            applyTree.updateAttachment(MonadicTree(q"(..$monadicTrees).map($f)"))
                            applyTree
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
                            parameter.attachments.get[MonadicTree] match {
                              case None =>
                                println(parameter)
                                println("None")
                                println()
                                innerLoop(restParameters,
                                          monadicTrees,
                                          argumentVals,
                                          transformedParameters.enqueue(parameter))
                              case Some(MonadicTree(monadicTree)) =>
                                println(parameter)
                                println("MonadicTree")
                                println()
                                val name = freshTermName()(currentFreshNameCreator)
                                innerLoop(
                                  restParameters,
                                  monadicTrees.enqueue(monadicTree),
                                  argumentVals.enqueue(q"val $name: ${parameter.tpe} = $EmptyTree"),
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

//                parameterLists.map { parameterList =>
//                  parameterList.map { parameter: Tree =>
//                    parameter.tpe.annotations.find(_.matches(monadicSymbol)) match {
//                      case None =>
//                      case Some(annotationInfo) =>
//                        val monadicTree = annotationInfo.argAtIndex(0)
//                    }
//
////                      .collectFirst {
////                      case q"new com.thoughtworks.each.monadic"
////                    }
//                  }
//                }
//                ???
              case other =>
                other
            }

          }
        }
      }
    )
  override val description: String = "Enable magic each function(or ! prefix)"

}
