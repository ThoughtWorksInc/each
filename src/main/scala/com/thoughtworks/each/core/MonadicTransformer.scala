/*
Copyright 2015 ThoughtWorks, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package com.thoughtworks.each.core

import com.thoughtworks.each.core.MonadicTransformer.ExceptionHandlingMode
import scala.language.higherKinds
import scala.annotation.tailrec

object MonadicTransformer {

  sealed trait ExceptionHandlingMode

  case object MonadThrowableMode extends ExceptionHandlingMode

  case object UnsupportedExceptionHandlingMode extends ExceptionHandlingMode

  case object MonadCatchIoMode extends ExceptionHandlingMode


}

abstract class MonadicTransformer[U <: scala.reflect.api.Universe]
(private[MonadicTransformer] val universe: U, exceptionHandlingMode: ExceptionHandlingMode) {


  // See https://groups.google.com/forum/#!topic/scala-language/g0-hbN5qerQ
  private implicit final class PartialFunctionAsExtractor[A, B](pf: PartialFunction[A, B]) {

    object Extractor {
      def unapply(a: A) = PartialFunction.condOpt(a)(pf)
    }

  }

  import MonadicTransformer._
  import universe._
  import Flag._


  protected sealed trait Instruction

  protected sealed case class Each(monadicTree: Tree) extends Instruction

  protected sealed case class Foreach(foldableOps: Tree, body: Function) extends Instruction

  protected sealed case class Map(traverseOps: Tree, body: Function, thisType: Type, hook: Tree => Tree) extends Instruction

  protected sealed case class FlatMap(traverseOps: Tree, resultType: Tree, body: Function, bind: Tree, thisType: Type, hook: Tree => Tree) extends Instruction

  protected sealed case class Filter(traverseOps: Tree, body: Function, monadPlus: Tree, thisType: Type, hook: Tree => Tree) extends Instruction

  // See https://issues.scala-lang.org/browse/SI-5712
  protected val instructionExtractor: PartialFunction[Tree, Instruction]

  // See https://issues.scala-lang.org/browse/SI-5712
  protected def fTree: Tree

  private val monadName = freshName("monad")

  private def monadTree: Tree = Ident(TermName(monadName))

  protected def freshName(name: String): String

  private object CpsTree {

    private final def typed(transformedTree: CpsTree, tpe: Type): CpsTree = {
      transformedTree.flatMap {
        x => PlainTree(Typed(x, TypeTree(tpe)), tpe)
      }
    }

    def apply(origin: Tree)(implicit forceAwait: Set[Name]): CpsTree = {
      origin match {
        case instructionExtractor.Extractor(instruction) => {
          instruction match {
            case FlatMap(opsTree, resultType, bodyFunctionTree@Function(List(valDefTree), bodyTree), bindTree, thisType, hook) => {
              CpsTree(opsTree).flatMap { x =>
                MonadTree(
                  Apply(
                    Apply(
                      TypeApply(
                        Select(x, TermName("traverseM")),
                        List(
                          fTree,
                          resultType
                        )
                      ),
                      List(treeCopy.Function(bodyFunctionTree, List(valDefTree), CpsTree(bodyTree).toReflectTree))
                    ),
                    List(monadTree, bindTree)
                  ),
                  thisType).flatMap { x =>
                  PlainTree(hook(x), origin.tpe)
                }
              }
            }
            case Map(opsTree, bodyFunctionTree@Function(List(valDefTree), bodyTree), thisType, hook) => {
              CpsTree(opsTree).flatMap { x =>
                MonadTree(
                  Apply(
                    Apply(
                      TypeApply(
                        Select(x, TermName("traverse")),
                        List(fTree, TypeTree(bodyTree.tpe))
                      ),
                      List(treeCopy.Function(bodyFunctionTree, List(valDefTree), CpsTree(bodyTree).toReflectTree))
                    ),
                    List(monadTree)
                  ),
                  thisType).flatMap { x =>
                  PlainTree(hook(x), origin.tpe)
                }
              }
            }
            case Foreach(opsTree, bodyFunctionTree@Function(List(valDefTree), bodyTree)) => {
              CpsTree(opsTree).flatMap { x =>
                MonadTree(
                  Apply(
                    Apply(
                      TypeApply(
                        Select(x, TermName("traverse_")),
                        List(fTree)
                      ),
                      List(treeCopy.Function(bodyFunctionTree, List(valDefTree), CpsTree(bodyTree).flatMap { x =>
                        PlainTree(Typed(x, TypeTree(definitions.UnitTpe)), definitions.UnitTpe)
                      }.toReflectTree))
                    ),
                    List(monadTree)
                  ),
                  definitions.UnitTpe)
              }
            }
            case Filter(opsTree, bodyFunctionTree@Function(List(valDefTree), bodyTree), monadPlusTree, thisType, hook) => {
              CpsTree(opsTree).flatMap { x =>
                MonadTree(
                  Apply(
                    Apply(
                      TypeApply(
                        Select(x, TermName("traverseM")),
                        List(
                          fTree,
                          valDefTree.tpt
                        )
                      ),
                      List(treeCopy.Function(bodyFunctionTree, List(valDefTree), (CpsTree(bodyTree).flatMap { x =>
                        PlainTree(
                          If(
                            x,
                            Apply(
                              Select(monadPlusTree, TermName("point")),
                              List(Ident(valDefTree.name))
                            ),
                            Select(monadPlusTree, TermName("empty"))
                          ),
                          valDefTree.tpt.tpe
                        )
                      }).toReflectTree))
                    ),
                    List(monadTree, monadPlusTree)
                  ),
                  thisType
                ).flatMap { x =>
                  PlainTree(hook(x), origin.tpe)
                }
              }
            }
            case Each(monadicTree) => {
              CpsTree(monadicTree).flatMap { x =>
                new MonadTree(x, origin.tpe)
              }
            }
          }
        }
        case Apply(method@Ident(name), parameters) if forceAwait(name) => {
          def transformParameters(untransformed: List[Tree], transformed: List[Tree]): CpsTree = {
            untransformed match {
              case Nil => {
                CpsTree(method).flatMap { transformedMethod =>
                  new MonadTree(treeCopy.Apply(origin, transformedMethod, transformed.reverse), origin.tpe)
                }
              }
              case head :: tail => {
                CpsTree(head).flatMap { transformedHead =>
                  transformParameters(tail, transformedHead :: transformed)
                }
              }
            }
          }
          transformParameters(parameters, Nil)
        }
        case Try(block, catches, finalizer) => {
          exceptionHandlingMode match {
            case UnsupportedExceptionHandlingMode => {
              new PlainTree(origin, origin.tpe)
            }
            case MonadThrowableMode => {
              val blockTree = CpsTree(block).toReflectTree
              val exceptionName = TermName(freshName("exception"))

              val tryCatch = Apply(
                Apply(
                  TypeApply(Select(monadTree, TermName("handleError")), List(TypeTree(origin.tpe))),
                  List(blockTree)
                ),
                List(
                  Function(
                    List(ValDef(Modifiers(PARAM), exceptionName, TypeTree(typeOf[_root_.java.lang.Throwable]), EmptyTree)),
                    Match(
                      Ident(exceptionName),
                      (for (caseTree@CaseDef(pat, guard, body) <- catches) yield {
                        treeCopy.CaseDef(caseTree, pat, guard, CpsTree(body).toReflectTree)
                      }) :+ CaseDef(
                        Ident(termNames.WILDCARD),
                        EmptyTree,
                        Apply(TypeApply(Select(monadTree, TermName("raiseError")), List(TypeTree(origin.tpe))), List(Ident(exceptionName)))
                      )))
                )
              )
              if (finalizer.isEmpty) {
                MonadTree(tryCatch, origin.tpe)
              } else {
                MonadTree(
                  Apply(
                    Apply(
                      TypeApply(Select(monadTree, TermName("handleError")), List(TypeTree(origin.tpe))),
                      List(tryCatch)
                    ),
                    List(
                      Function(
                        List(ValDef(Modifiers(PARAM), exceptionName, TypeTree(typeOf[_root_.java.lang.Throwable]), EmptyTree)),
                        (CpsTree(finalizer).flatMap { transformedFinalizer =>
                          MonadTree(
                            Block(List(transformedFinalizer),
                              Apply(TypeApply(Select(monadTree, TermName("raiseError")), List(TypeTree(origin.tpe))), List(Ident(exceptionName))
                              )), origin.tpe)
                        }).toReflectTree
                      )
                    )
                  ),
                  origin.tpe
                ).flatMap { transformedTryCatch =>
                  (CpsTree(finalizer).flatMap { transformedFinalizer =>
                    PlainTree(
                      Block(List(transformedFinalizer),
                        transformedTryCatch
                      ), origin.tpe)
                  })
                }
              }

            }
            case MonadCatchIoMode => {
              val tryCatch = catches.foldLeft(CpsTree(block).toReflectTree) { (tryF, cd) =>
                val CaseDef(pat, guard, body) = cd

                val exceptionName = TermName(freshName("exception"))
                val catcherResultName = TermName(freshName("catcherResult"))

                Apply(
                  Apply(
                    Apply(
                      TypeApply(
                        Select(reify(_root_.scalaz.effect.MonadCatchIO).tree, TermName("catchSome")),
                        List(
                          fTree,
                          TypeTree(origin.tpe),
                          AppliedTypeTree(fTree, List(TypeTree(origin.tpe))))),
                      List(tryF)),
                    List(
                      Function(
                        List(ValDef(Modifiers(PARAM), exceptionName, TypeTree(typeOf[_root_.java.lang.Throwable]), EmptyTree)),
                        Match(
                          Ident(exceptionName),
                          List(
                            treeCopy.CaseDef(cd, pat, guard, Apply(reify(_root_.scala.Some).tree, List(CpsTree(body).toReflectTree))),
                            CaseDef(Ident(termNames.WILDCARD), EmptyTree, reify(_root_.scala.None).tree)))),
                      Function(
                        List(
                          ValDef(
                            Modifiers(PARAM),
                            catcherResultName,
                            AppliedTypeTree(fTree, List(TypeTree(origin.tpe))),
                            EmptyTree)),
                        Ident(catcherResultName)))),
                  List(monadTree))

              }
              if (finalizer.isEmpty) {
                MonadTree(tryCatch, origin.tpe)
              } else {
                MonadTree(
                  Apply(
                    Apply(
                      Select(reify(_root_.scalaz.effect.MonadCatchIO).tree, TermName("ensuring")),
                      List(tryCatch, CpsTree(finalizer).toReflectTree)),
                    List(monadTree)),
                  origin.tpe)
              }
            }
          }
        }
        case Select(instance, field) => {
          CpsTree(instance).flatMap { x =>
            new PlainTree(treeCopy.Select(origin, x, field), origin.tpe)
          }
        }
        case TypeApply(method, parameters) => {
          CpsTree(method).flatMap { x =>
            new PlainTree(TypeApply(x, parameters), origin.tpe)
          }
        }
        case Apply(method, parameters) => {
          CpsTree(method).flatMap { transformedMethod =>
            def transformParameters(untransformed: List[Tree], transformed: List[Tree]): CpsTree = {
              untransformed match {
                case Nil => {
                  new PlainTree(treeCopy.Apply(origin, transformedMethod, transformed.reverse), origin.tpe)
                }
                case head :: tail => {
                  CpsTree(head).flatMap { transformedHead =>
                    transformParameters(tail, transformedHead :: transformed)
                  }
                }
              }
            }
            transformParameters(parameters, Nil)
          }
        }
        case Block(stats, expr) => {
          def transformStats(untransformed: List[Tree]): CpsTree = {
            untransformed match {
              case Nil => {
                CpsTree(expr)
              }
              case head :: tail => {
                val transformedTree = CpsTree(head)
                transformedTree.flatMap { transformedHead =>

                  // Avoid warning: a pure expression does nothing in statement position
                  @tailrec
                  def isDiscardable(transformedTree: CpsTree): Boolean = {
                    transformedTree match {
                      case _: OpenTree => false
                      case BlockTree(_, tail) => isDiscardable(tail)
                      case _: MonadTree => true
                      case _: PlainTree => false
                    }
                  }
                  if (isDiscardable(transformedTree)) {
                    transformStats(tail)
                  } else {
                    transformStats(tail) match {
                      case BlockTree(prefix, tail) => {
                        BlockTree(transformedHead :: prefix, tail)
                      }
                      case transformedTail@(_: MonadTree | _: OpenTree | _: PlainTree) => {
                        BlockTree(transformedHead :: Nil, transformedTail)
                      }
                    }
                  }
                }
              }
            }
          }
          new MonadTree(transformStats(stats).toReflectTree, origin.tpe)
        }
        case ValDef(mods, name, tpt, rhs) => {
          CpsTree(rhs).flatMap { x =>
            new PlainTree(treeCopy.ValDef(origin, mods, name, tpt, x), origin.tpe)
          }
        }
        case Assign(left, right) => {
          CpsTree(right).flatMap { x =>
            new PlainTree(treeCopy.Assign(origin, left, x), origin.tpe)
          }
        }
        case Match(selector, cases) => {
          val selectorName = TermName(freshName("selector"))
          new MonadTree(
            Apply(
              Apply(
                TypeApply(
                  Select(monadTree, TermName("bind")),
                  List(TypeTree(selector.tpe), TypeTree(origin.tpe))),
                List(CpsTree(selector).toReflectTree)),
              List(
                Function(
                  List(ValDef(Modifiers(PARAM), selectorName, TypeTree(selector.tpe), EmptyTree)),
                  treeCopy.Match(
                    origin,
                    Ident(selectorName),
                    for {
                      cd@CaseDef(pat, guard, body) <- cases
                    } yield
                    treeCopy.CaseDef(cd, pat, guard, CpsTree(body).toReflectTree))))),
            origin.tpe)
        }
        case If(cond, thenp, elsep) => {
          new MonadTree(
            treeCopy.Apply(
              origin,
              TypeApply(
                Select(monadTree, TermName("ifM")),
                List(TypeTree(origin.tpe))),
              List(
                CpsTree(cond).toReflectTree,
                CpsTree(thenp).toReflectTree,
                CpsTree(elsep).toReflectTree)),
            origin.tpe)
        }
        case Typed(expr, tpt@Ident(typeNames.WILDCARD_STAR)) => {
          CpsTree(expr).flatMap { x =>
            new PlainTree(treeCopy.Typed(origin, x, tpt), origin.tpe)
          }
        }
        case Typed(expr, tpt) => {
          CpsTree(expr).flatMap { x =>
            new PlainTree(treeCopy.Typed(origin, x, TypeTree(tpt.tpe)), origin.tpe)
          }
        }
        case Annotated(annot, arg) => {
          CpsTree(arg).flatMap { x =>
            new PlainTree(Annotated(annot, x), origin.tpe)
          }
        }
        case LabelDef(name1, List(), If(condition, block@Block(body, Apply(Ident(name2), List())), Literal(Constant(()))))
          if name1 == name2 => {
          new MonadTree(
            Apply(
              TypeApply(
                Select(monadTree, TermName("whileM_")),
                List(TypeTree(origin.tpe))),
              List(
                CpsTree(condition).toReflectTree,
                CpsTree(treeCopy.Block(block, body, Literal(Constant(())))).toReflectTree)),
            origin.tpe)
        }
        case LabelDef(name1, List(), block@Block(body, If(condition, Apply(Ident(name2), List()), Literal(Constant(())))))
          if name1 == name2 => {
          new MonadTree(
            Block(List(
              ValDef(Modifiers(), name1, TypeTree(), CpsTree(treeCopy.Block(block, body, Literal(Constant(())))).toReflectTree)),
              Apply(
                Apply(
                  TypeApply(
                    Select(monadTree, TermName("bind")),
                    List(TypeTree(definitions.UnitTpe), TypeTree(origin.tpe))),
                  List(Ident(name1))),
                List(
                  Function(
                    List(ValDef(Modifiers(PARAM), TermName(freshName("ignoredParameter")), TypeTree(definitions.UnitTpe), EmptyTree)),
                    Apply(
                      TypeApply(
                        Select(monadTree, TermName("whileM_")),
                        List(TypeTree(origin.tpe))
                      ),
                      List(
                        CpsTree(condition).toReflectTree,
                        Ident(name1)
                      )
                    )
                  )
                )
              )
            ),
            origin.tpe)
        }
        case Throw(throwable) => {
          exceptionHandlingMode match {
            case MonadThrowableMode => {
              CpsTree(throwable).flatMap { x =>
                new MonadTree(Apply(TypeApply(Select(monadTree, TermName("raiseError")), List(TypeTree(origin.tpe))), List(x)), origin.tpe)
              }
            }
            case UnsupportedExceptionHandlingMode | MonadCatchIoMode => {
              CpsTree(throwable).flatMap { x =>
                new PlainTree(treeCopy.Throw(origin, x), origin.tpe)
              }
            }
          }
        }
        case EmptyTree | _: Return | _: New | _: Ident | _: Literal | _: Super | _: This | _: TypTree | _: New | _: TypeDef | _: Function | _: DefDef | _: ClassDef | _: ModuleDef | _: Import | _: ImportSelector => {
          new PlainTree(origin, origin.tpe)
        }
      }
    }
  }

  private sealed abstract class CpsTree {

    def toReflectTree: Tree

    def tpe: Type

    def flatMap(f: Tree => CpsTree): CpsTree

  }

  private final case class OpenTree(prefix: CpsTree, parameter: ValDef, inner: CpsTree) extends CpsTree {

    override final def toReflectTree: Tree = {
      inner match {
        case PlainTree(plain, _) => {
          Apply(
            Apply(
              Select(monadTree, TermName("map")),
              List(prefix.toReflectTree)),
            List(
              Function(List(parameter), plain)))
        }
        case _ => {
          val innerTree = inner.toReflectTree
          Apply(
            Apply(
              Select(monadTree, TermName("bind")),
              List(prefix.toReflectTree)),
            List(
              Function(List(parameter), innerTree)))
        }
      }
    }

    override final def tpe = inner.tpe

    override final def flatMap(f: Tree => CpsTree): CpsTree = {
      new OpenTree(prefix, parameter, inner.flatMap(f))
    }

  }

  private final case class BlockTree(prefix: List[Tree], tail: CpsTree) extends CpsTree {

    override final def tpe = tail.tpe

    override final def toReflectTree: Tree = {
      Block(prefix, tail.toReflectTree)
    }

    override final def flatMap(f: Tree => CpsTree): CpsTree = {
      BlockTree(prefix, tail.flatMap(f))
    }

  }

  private final case class MonadTree(override final val toReflectTree: Tree, override final val tpe: Type) extends CpsTree {

    override final def flatMap(f: Tree => CpsTree): CpsTree = {
      val newId = TermName(freshName("element"))
      OpenTree(MonadTree.this, ValDef(Modifiers(PARAM), newId, TypeTree(tpe), EmptyTree), f(Ident(newId)))
    }

  }

  private final case class PlainTree(tree: Tree, tpe: Type) extends CpsTree {

    override final def toReflectTree: Tree = {
      Apply(
        Select(monadTree, TermName("point")),
        List(tree))
    }

    override final def flatMap(f: Tree => CpsTree): CpsTree = {
      f(tree)
    }

  }

  final def transform(origin: Tree, monad: Tree): Tree = {
    Block(
      List(
        ValDef(NoMods, TermName(monadName), TypeTree(monad.tpe), monad)
      ),
      CpsTree(origin)(Set.empty).toReflectTree
    )
  }
}