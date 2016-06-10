package com.thoughtworks.sde.core

import scala.annotation.{compileTimeOnly, tailrec}
import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scalaz.{Bind, Foldable, MonadPlus, Traverse}
import macrocompat.bundle


/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object MonadicBackend {

  sealed trait ExceptionHandlingMode

  object ExceptionHandlingMode {

    case object MonadThrowableMode extends ExceptionHandlingMode

    case object UnsupportedExceptionHandlingMode extends ExceptionHandlingMode

    case object MonadCatchIoMode extends ExceptionHandlingMode

    // TODO: CatchableMode

    private[MonadicBackend] def fromModeName(modeName: String) = {
      import reflect.runtime.universe._
      reflect.runtime.currentMirror.reflectModule(typeOf[this.type].member(newTermName(modeName)).asModule).instance.asInstanceOf[ExceptionHandlingMode]
    }

  }

  def run[M[_[_]], F[_], A](typeClass: M[F], exceptionHandlingMode: MonadicBackend.ExceptionHandlingMode, body: => A): F[A] = macro MacroBundle.run

  @bundle
  private[thoughtworks] final class MacroBundle(private[MonadicBackend] val c: whitebox.Context) {

    import c.universe._
    import Flag._


    /**
      * @param fTree See https://issues.scala-lang.org/browse/SI-5712
      */
    final class MonadicContext(exceptionHandlingMode: ExceptionHandlingMode, fTree: Tree) {

      // See https://groups.google.com/forum/#!topic/scala-language/g0-hbN5qerQ
      private implicit final class PartialFunctionAsExtractor[A, B](pf: PartialFunction[A, B]) {

        object Extractor {
          def unapply(a: A) = PartialFunction.condOpt(a)(pf)
        }

      }

      import ExceptionHandlingMode._

      private val eachMethodSymbol = typeOf[_root_.com.thoughtworks.sde.core.MonadicBackend.Instructions.type].member(TermName("each"))
      private val foreachMethodSymbol = typeOf[_root_.com.thoughtworks.sde.core.MonadicBackend.Instructions.type].member(TermName("foreach"))
      private val mapMethodSymbol = typeOf[_root_.com.thoughtworks.sde.core.MonadicBackend.Instructions.type].member(TermName("map"))
      private val flatMapMethodSymbol = typeOf[_root_.com.thoughtworks.sde.core.MonadicBackend.Instructions.type].member(TermName("flatMap"))
      private val filterMethodSymbol = typeOf[_root_.com.thoughtworks.sde.core.MonadicBackend.Instructions.type].member(TermName("filter"))

      private val monadName = c.freshName("monad")

      private def monadTree: Tree = Ident(TermName(monadName))

      private object CpsTree {

        private final def typed(transformedTree: CpsTree, tpe: Type): CpsTree = {
          transformedTree.flatMap {
            x => PlainTree(Typed(x, TypeTree(tpe)), tpe)
          }
        }

        // Avoid warning: a pure expression does nothing in statement position
        @tailrec
        private def isDiscardable(transformedTree: CpsTree): Boolean = {
          transformedTree match {
            case OpenTree(_, _, inner) => isDiscardable(inner)
            case BlockTree(_, tail) => isDiscardable(tail)
            case _: MonadTree => true
            case _: PlainTree => false
          }
        }

        def apply(origin: Tree)(implicit forceAwait: Set[Name]): CpsTree = {
          origin match {
            case q"""$method[$f, $a]($fa)""" if method.symbol == eachMethodSymbol =>
              CpsTree(fa).flatMap { x =>
                new MonadTree(x, origin.tpe)
              }
            case q"""$method[$fType, $a, $u]($fa, $foldable, $f)""" if method.symbol == foreachMethodSymbol =>
              CpsTree(fa).flatMap { faValue =>
                f match {
                  case Function(List(valDef), body) =>
                    MonadTree(
                      q"""
                    $foldable.traverse_[$fTree, $a, $u](${
                        treeCopy.Function(f, List(valDef), CpsTree(body).toReflectTree)
                      })($monadTree)
                  """, origin.tpe)
                  case _ =>
                    CpsTree(f).flatMap { fValue =>
                      PlainTree(
                        q"""
                      $foldable.traverse_[_root_.scalaz.Id.Id, $a, $u]($faValue)($fValue)(_root_.scalaz.Id.id)
                    """, origin.tpe)
                    }
                }
              }
            case q"""$method[$fType, $a, $b]($fa, $traverse, $f)""" if method.symbol == mapMethodSymbol =>
              CpsTree(fa).flatMap { faValue =>
                f match {
                  case Function(List(valDef), body) =>
                    MonadTree(
                      q"""
                    $traverse.traverse[$fTree, $b](${
                        treeCopy.Function(f, List(valDef), CpsTree(body).toReflectTree)
                      })($monadTree)
                  """, origin.tpe)
                  case _ =>
                    CpsTree(f).flatMap { fValue =>
                      PlainTree(q"""$traverse.map[$a, $b]($faValue)($fValue)""", origin.tpe)
                    }
                }
              }
            case q"""$method[$fType, $a, $b]($fa, $traverse, $bind, $f)""" if method.symbol == flatMapMethodSymbol =>
              CpsTree(fa).flatMap { faValue =>
                f match {
                  case Function(List(valDef), body) =>
                    MonadTree(
                      q"""
                    $traverse.traverseM[$fTree, $b](${
                        treeCopy.Function(f, List(valDef), CpsTree(body).toReflectTree)
                      })($monadTree, $bind)
                  """, origin.tpe)
                  case _ =>
                    CpsTree(f).flatMap { fValue =>
                      PlainTree(q"""$bind.bind[$a, $b]($faValue)($fValue)""", origin.tpe)
                    }
                }
              }
            case q"""$method[$fType, $a]($fa, $traverse, $monadPlus, $f)""" if method.symbol == filterMethodSymbol =>
              CpsTree(fa).flatMap { faValue =>
                f match {
                  case Function(List(valDef), body) =>
                    MonadTree(
                      q"""
                    $traverse.traverseM[$fTree, $a](${
                        treeCopy.Function(f, List(valDef), CpsTree(body).flatMap { condition =>
                          PlainTree(
                            q"""
                        if ($condition) $monadPlus.point[$a](${valDef.name}) else $monadPlus.empty
                      """, a.tpe)
                        }.toReflectTree)
                      })($monadTree)
                  """, origin.tpe)
                  case _ =>
                    CpsTree(f).flatMap { fValue =>
                      PlainTree(
                        q"""
                      $monadPlus.filter[$a]($faValue)($fValue)
                    """, origin.tpe)
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
                  val exceptionName = TermName(c.freshName("exception"))

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
                    val finalizerCpsTree = CpsTree(finalizer)
                    MonadTree(
                      Apply(
                        Apply(
                          TypeApply(Select(monadTree, TermName("handleError")), List(TypeTree(origin.tpe))),
                          List(tryCatch)
                        ),
                        List(
                          Function(
                            List(ValDef(Modifiers(PARAM), exceptionName, TypeTree(typeOf[_root_.java.lang.Throwable]), EmptyTree)),
                            finalizerCpsTree.flatMap { transformedFinalizer =>
                              if (isDiscardable(finalizerCpsTree)) {
                                MonadTree(
                                  Apply(TypeApply(Select(monadTree, TermName("raiseError")), List(TypeTree(origin.tpe))), List(Ident(exceptionName))),
                                  origin.tpe)
                              } else {
                                MonadTree(
                                  Block(
                                    List(transformedFinalizer),
                                    Apply(TypeApply(Select(monadTree, TermName("raiseError")), List(TypeTree(origin.tpe))), List(Ident(exceptionName))
                                    )), origin.tpe)
                              }
                            }.toReflectTree
                          )
                        )
                      ),
                      origin.tpe
                    ).flatMap { transformedTryCatch =>
                      finalizerCpsTree.flatMap { transformedFinalizer =>
                        if (isDiscardable(finalizerCpsTree)) {
                          PlainTree(transformedTryCatch, origin.tpe)
                        } else {
                          PlainTree(Block(List(transformedFinalizer), transformedTryCatch), origin.tpe)
                        }
                      }
                    }
                  }

                }
                case MonadCatchIoMode => {
                  val tryCatch = catches.foldLeft(CpsTree(block).toReflectTree) { (tryF, cd) =>
                    val CaseDef(pat, guard, body) = cd

                    val exceptionName = TermName(c.freshName("exception"))
                    val catcherResultName = TermName(c.freshName("catcherResult"))

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
              val selectorName = TermName(c.freshName("selector"))
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
                        List(ValDef(Modifiers(PARAM), TermName(c.freshName("ignoredParameter")), TypeTree(definitions.UnitTpe), EmptyTree)),
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
          val newId = TermName(c.freshName("element"))
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

    def run(typeClass: Tree, exceptionHandlingMode: Tree, body: Tree): Tree = {
      val q"$method[$m, $f, $a](..$arguments)" = c.macroApplication
      val mode = ExceptionHandlingMode.fromModeName(exceptionHandlingMode.tpe.typeSymbol.name.toString)
      val transformer = new MonadicContext(mode, f)

      val result = transformer.transform(body, typeClass)
      c.untypecheck(result)
      result
    }

  }

  object Instructions {

    @compileTimeOnly("Instructions must be inside a SDE block")
    def each[F[_], A](fa: F[A]): A = ???

    @compileTimeOnly("Instructions must be inside a SDE block")
    def foreach[F[_], A, U](fa: F[A], foldable: Foldable[F], body: A => U): Unit = ???

    @compileTimeOnly("Instructions must be inside a SDE block")
    def map[F[_], A, B](fa: F[A], traverse: Traverse[F], body: A => B): F[B] = ???

    @compileTimeOnly("Instructions must be inside a SDE block")
    def flatMap[F[_], A, B](fa: F[A], traverse: Traverse[F], bind: Bind[F], body: A => F[B]): F[B] = ???

    @compileTimeOnly("Instructions must be inside a SDE block")
    def filter[F[_], A](fa: F[A], traverse: Traverse[F], monadPlus: MonadPlus[F], body: A => Boolean): F[A] = ???

  }

}
