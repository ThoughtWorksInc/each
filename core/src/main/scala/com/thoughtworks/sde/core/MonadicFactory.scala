package com.thoughtworks.sde.core

import scala.annotation.{compileTimeOnly, tailrec}
import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scalaz.{Bind, Foldable, MonadPlus, Traverse}
import macrocompat.bundle

class MonadicFactory[M0[_[_]], F0[_]] {
  type M[F[_]] = M0[F]
  type F[A] = F0[A]

  def apply[A](body: => A)(implicit typeClass: M[F]): F[A] = macro MonadicFactory.MacroBundle.apply

  class WithTypeClass(implicit val typeClass: M[F]) {
    type M[F[_]] = M0[F]
    type F[A] = F0[A]
    def apply[A](body: => A): F[A] = macro MonadicFactory.MacroBundle.withTypeClassApply
  }

  def withTypeClass(implicit typeClass: M[F]) = new WithTypeClass

}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object MonadicFactory {

  private[MonadicFactory] sealed trait ExceptionHandlingMode

  private[MonadicFactory] case object MonadThrowableMode extends ExceptionHandlingMode

  private[MonadicFactory] case object UnsupportedExceptionHandlingMode extends ExceptionHandlingMode

  private[MonadicFactory] case object MonadCatchIoMode extends ExceptionHandlingMode

  // TODO: CatchableMode

  @bundle
  private[thoughtworks] final class MacroBundle(private[MonadicFactory] val c: blackbox.Context) {

    import c.universe._
    import Flag._

    private val monadErrorClassSymbol = symbolOf[_root_.scalaz.MonadError[_root_.scala.List, _]]
    private val monadCatchIoClassSymbol = symbolOf[_root_.scalaz.effect.MonadCatchIO[_root_.scala.List]]

    /**
      * @param fTree See https://issues.scala-lang.org/browse/SI-5712
      */
    private[MacroBundle] final class MonadicContext(exceptionHandlingMode: ExceptionHandlingMode, fTree: Tree, monadTree: Tree) {

      sealed abstract class CpsTree {

        def toReflectTree: Tree

        final def toUntypedTree = c.untypecheck(toReflectTree)

        def tpe: Type

        def flatMap(f: Tree => CpsTree): CpsTree

      }

      final case class OpenTree(prefix: CpsTree, parameter: ValDef, inner: CpsTree) extends CpsTree {

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

      final case class BlockTree(prefix: List[Tree], tail: CpsTree) extends CpsTree {

        override final def tpe = tail.tpe

        override final def toReflectTree: Tree = {
          Block(prefix, tail.toReflectTree)
        }

        override final def flatMap(f: Tree => CpsTree): CpsTree = {
          BlockTree(prefix, tail.flatMap(f))
        }

      }

      final case class MonadTree(reflectTree: Tree, override final val tpe: Type) extends CpsTree {

        override final def toReflectTree = reflectTree

        override final def flatMap(f: Tree => CpsTree): CpsTree = {
          val newId = TermName(c.freshName("element"))
          OpenTree(MonadTree.this, ValDef(Modifiers(PARAM), newId, TypeTree(tpe), EmptyTree), f(Ident(newId)))
        }

      }

      final case class PlainTree(tree: Tree, tpe: Type) extends CpsTree {

        override final def toReflectTree: Tree = {
          Apply(
            Select(monadTree, TermName("point")),
            List(tree))
        }

        override final def flatMap(f: Tree => CpsTree): CpsTree = {
          f(tree)
        }

      }

      private val eachMethodSymbol = typeOf[_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.type].member(TermName("each"))
      private val foreachMethodSymbol = typeOf[_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.type].member(TermName("foreach"))
      private val mapMethodSymbol = typeOf[_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.type].member(TermName("map"))
      private val flatMapMethodSymbol = typeOf[_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.type].member(TermName("flatMap"))
      private val filterMethodSymbol = typeOf[_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.type].member(TermName("filter"))

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

      def buildCpsTree(origin: Tree)(implicit forceAwait: Set[Name]): CpsTree = {
        origin match {
          case q"""$method[$f, $a]($fa)""" if method.symbol == eachMethodSymbol =>
            buildCpsTree(fa).flatMap { x =>
              new MonadTree(x, origin.tpe)
            }
          case q"""$method[$fType, $a, $u]($fa, $foldable, $f)""" if method.symbol == foreachMethodSymbol =>
            buildCpsTree(fa).flatMap { faValue =>
              f match {
                case Function(List(valDef), body) =>
                  MonadTree(
                    atPos(origin.pos) {
                      q"""
                        $foldable.traverse_[$fTree, $a, $u]($faValue)(${
                          treeCopy.Function(f, List(valDef), buildCpsTree(body).toUntypedTree)
                        })($monadTree)
                      """
                    },
                    origin.tpe
                  )
                case _ =>
                  buildCpsTree(f).flatMap { fValue =>
                    PlainTree(
                      atPos(origin.pos) {
                        q"""
                          $foldable.traverse_[_root_.scalaz.Id.Id, $a, $u]($faValue)($fValue)(_root_.scalaz.Id.id)
                        """
                      }, origin.tpe)
                  }
              }
            }
          case q"""$method[$fType, $a, $b]($fa, $traverse, $f)""" if method.symbol == mapMethodSymbol =>
            buildCpsTree(fa).flatMap { faValue =>
              f match {
                case Function(List(valDef), body) =>
                  MonadTree(
                    atPos(origin.pos) {
                      q"""
                        _root_.scalaz.syntax.traverse.ToTraverseOps[$fType, $a]($faValue)($traverse).traverse[$fTree, $b](${
                          treeCopy.Function(f, List(valDef), buildCpsTree(body).toUntypedTree)
                        })($monadTree)
                      """
                    },
                    origin.tpe
                  )
                case _ =>
                  buildCpsTree(f).flatMap { fValue =>
                    PlainTree(atPos(origin.pos)(q"$traverse.map[$a, $b]($faValue)($fValue)"), origin.tpe)
                  }
              }
            }
          case q"""$method[$fType, $a, $b]($fa, $traverse, $bind, $f)""" if method.symbol == flatMapMethodSymbol =>
            buildCpsTree(fa).flatMap { faValue =>
              f match {
                case Function(List(valDef), body) =>
                  MonadTree(
                    atPos(origin.pos) {
                      q"""
                        $traverse.traverseM[$a, $fTree, $b]($faValue)(${
                          treeCopy.Function(f, List(valDef), buildCpsTree(body).toUntypedTree)
                        })($monadTree, $bind)
                      """
                    },
                    origin.tpe
                  )
                case _ =>
                  buildCpsTree(f).flatMap { fValue =>
                    PlainTree(atPos(origin.pos)(q"$bind.bind[$a, $b]($faValue)($fValue)"), origin.tpe)
                  }
              }
            }
          case q"""$method[$fType, $a]($fa, $traverse, $monadPlus, $f)""" if method.symbol == filterMethodSymbol =>
            buildCpsTree(fa).flatMap { faValue =>
              f match {
                case Function(List(valDef), body) =>
                  MonadTree(
                    atPos(origin.pos) {
                      q"""
                        $traverse.traverseM[$a, $fTree, $a]($faValue)(
                          ${
                            treeCopy.Function(f, List(valDef),
                              (buildCpsTree(body).flatMap { condition =>
                                PlainTree(
                                  q"""
                                    if ($condition) $monadPlus.point[$a](${valDef.name}) else $monadPlus.empty
                                  """,
                                  valDef.tpt.tpe
                                )
                              }).toUntypedTree
                            )
                          }
                        )($monadTree, $monadPlus)
                      """
                    },
                    origin.tpe
                  )
                case _ =>
                  buildCpsTree(f).flatMap { fValue =>
                    PlainTree(atPos(origin.pos)(q"$monadPlus.filter[$a]($faValue)($fValue)"), origin.tpe)
                  }
              }
            }
          case Apply(method@Ident(name), parameters) if forceAwait(name) => {
            def transformParameters(untransformed: List[Tree], transformed: List[Tree]): CpsTree = {
              untransformed match {
                case Nil => {
                  buildCpsTree(method).flatMap { transformedMethod =>
                    new MonadTree(treeCopy.Apply(origin, transformedMethod, transformed.reverse), origin.tpe)
                  }
                }
                case head :: tail => {
                  buildCpsTree(head).flatMap { transformedHead =>
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
                val blockTree = buildCpsTree(block).toReflectTree
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
                          treeCopy.CaseDef(caseTree, pat, guard, buildCpsTree(body).toReflectTree)
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
                  val finalizerCpsTree = buildCpsTree(finalizer)
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
                val tryCatch = catches.foldLeft(buildCpsTree(block).toReflectTree) { (tryF, cd) =>
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
                              treeCopy.CaseDef(cd, pat, guard, Apply(reify(_root_.scala.Some).tree, List(buildCpsTree(body).toReflectTree))),
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
                        List(tryCatch, buildCpsTree(finalizer).toReflectTree)),
                      List(monadTree)),
                    origin.tpe)
                }
              }
            }
          }
          case Select(instance, field) => {
            buildCpsTree(instance).flatMap { x =>
              new PlainTree(treeCopy.Select(origin, x, field), origin.tpe)
            }
          }
          case TypeApply(method, parameters) => {
            buildCpsTree(method).flatMap { x =>
              new PlainTree(TypeApply(x, parameters), origin.tpe)
            }
          }
          case Apply(method, parameters) => {
            buildCpsTree(method).flatMap { transformedMethod =>
              def transformParameters(untransformed: List[Tree], transformed: List[Tree]): CpsTree = {
                untransformed match {
                  case Nil => {
                    new PlainTree(treeCopy.Apply(origin, transformedMethod, transformed.reverse), origin.tpe)
                  }
                  case head :: tail => {
                    buildCpsTree(head).flatMap { transformedHead =>
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
                  buildCpsTree(expr)
                }
                case head :: tail => {
                  val transformedTree = buildCpsTree(head)
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
            buildCpsTree(rhs).flatMap { x =>
              new PlainTree(treeCopy.ValDef(origin, mods, name, tpt, x), origin.tpe)
            }
          }
          case Assign(left, right) => {
            buildCpsTree(right).flatMap { x =>
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
                  List(buildCpsTree(selector).toReflectTree)),
                List(
                  Function(
                    List(ValDef(Modifiers(PARAM), selectorName, TypeTree(selector.tpe), EmptyTree)),
                    treeCopy.Match(
                      origin,
                      Ident(selectorName),
                      for {
                        cd@CaseDef(pat, guard, body) <- cases
                      } yield
                        treeCopy.CaseDef(cd, pat, guard, buildCpsTree(body).toReflectTree))))),
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
                  buildCpsTree(cond).toReflectTree,
                  buildCpsTree(thenp).toReflectTree,
                  buildCpsTree(elsep).toReflectTree)),
              origin.tpe)
          }
          case Typed(expr, tpt@Ident(typeNames.WILDCARD_STAR)) => {
            buildCpsTree(expr).flatMap { x =>
              new PlainTree(atPos(origin.pos)(Typed(x, tpt)), origin.tpe)
            }
          }
          case Typed(expr, tpt) => {
            buildCpsTree(expr).flatMap { x =>
              new PlainTree(atPos(origin.pos)(Typed(x, TypeTree(tpt.tpe))), origin.tpe)
            }
          }
          case Annotated(annot, arg) => {
            buildCpsTree(arg).flatMap { x =>
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
                  buildCpsTree(condition).toReflectTree,
                  buildCpsTree(treeCopy.Block(block, body, Literal(Constant(())))).toReflectTree)),
              origin.tpe)
          }
          case LabelDef(name1, List(), block@Block(body, If(condition, Apply(Ident(name2), List()), Literal(Constant(())))))
            if name1 == name2 => {
            new MonadTree(
              Block(List(
                ValDef(Modifiers(), name1, TypeTree(), buildCpsTree(treeCopy.Block(block, body, Literal(Constant(())))).toReflectTree)),
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
                          buildCpsTree(condition).toReflectTree,
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
                buildCpsTree(throwable).flatMap { x =>
                  new MonadTree(Apply(TypeApply(Select(monadTree, TermName("raiseError")), List(TypeTree(origin.tpe))), List(x)), origin.tpe)
                }
              }
              case UnsupportedExceptionHandlingMode | MonadCatchIoMode => {
                buildCpsTree(throwable).flatMap { x =>
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

    def apply(body: Tree)(typeClass: Tree): Tree = {
      val q"$partialAppliedMonadic.apply[$a]($body)($typeClass)" = c.macroApplication

      val mode = if (typeClass.tpe.baseType(monadErrorClassSymbol) != NoType) {
        MonadThrowableMode
      } else if (typeClass.tpe.baseType(monadCatchIoClassSymbol) != NoType) {
        MonadCatchIoMode
      } else {
        UnsupportedExceptionHandlingMode
      }


      val partialAppliedMonadicName = TermName(c.freshName("partialAppliedMonadic"))
      val monadName = TermName(c.freshName("monad"))
      val monadTree = q"$monadName"

      val cpsTreeBuilder = new MonadicContext(mode, tq"$partialAppliedMonadicName.F", monadTree)

      // c.info(c.enclosingPosition, showCode(body), true)
      val result =
        q"""{
          val $partialAppliedMonadicName = $partialAppliedMonadic
          val $monadName = $typeClass
          ${cpsTreeBuilder.buildCpsTree(body)(Set.empty).toUntypedTree}
        }"""
      // c.info(c.enclosingPosition, showCode(result), true)
      result
    }

    def withTypeClassApply(body: Tree): Tree = {
      val q"$withTypeClass.apply[$a]($body)" = c.macroApplication

      val typeClassType = internal.singleType(withTypeClass.tpe, withTypeClass.tpe.member(TermName("typeClass")))

      val mode = if (typeClassType.baseType(monadErrorClassSymbol) != NoType) {
        MonadThrowableMode
      } else if (typeClassType.baseType(monadCatchIoClassSymbol) != NoType) {
        MonadCatchIoMode
      } else {
        UnsupportedExceptionHandlingMode
      }


      val withTypeClassName = TermName(c.freshName("partialAppliedMonadic"))
      val monadTree = q"$withTypeClassName.typeClass"

      val cpsTreeBuilder = new MonadicContext(mode, tq"$withTypeClassName.F", monadTree)

      // c.info(c.enclosingPosition, showCode(body), true)
      val result =
        q"""{
          val $withTypeClassName = $withTypeClass
          ${cpsTreeBuilder.buildCpsTree(body)(Set.empty).toUntypedTree}
        }"""
      // c.info(c.enclosingPosition, showCode(result), true)
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
