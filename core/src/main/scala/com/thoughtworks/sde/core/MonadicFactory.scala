package com.thoughtworks.sde.core

import com.thoughtworks.template

import scala.annotation.{compileTimeOnly, tailrec}
import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.{blackbox, whitebox}
import macrocompat.bundle

trait MonadicFactory[F[_]] {
  def apply[A](body: A): F[A] = macro MonadicFactory.MacroBundle.apply[A]
}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object MonadicFactory { // TODO: rename

  def apply[F[_]] = new MonadicFactory[F] {}

  @bundle
  private[thoughtworks] final class MacroBundle(private[MonadicFactory] val c: whitebox.Context) {

    import c.universe._
    import Flag._

//    private val monadErrorClassSymbol = symbolOf[_root_.scalaz.MonadError[_root_.scala.List, _]]
//    private val monadCatchIoClassSymbol = symbolOf[_root_.scalaz.effect.MonadCatchIO[_root_.scala.List]]

    sealed abstract class CpsTree {

      def toReflectTree: Tree

      final def toUntypedTree = c.untypecheck(toReflectTree)

      def tpe: Type

      def flatMap(f: Tree => CpsTree): CpsTree

    }

    final case class OpenTree(prefix: CpsTree, parameter: ValDef, inner: CpsTree) extends CpsTree {

      override def toReflectTree: Tree = {
        inner match {
          case PlainTree(plain, _) => {
            q"${prefix.toReflectTree}.map(${Function(List(parameter), plain)})"
          }
          case _ => {
            val innerTree = inner.toReflectTree
            q"${prefix.toReflectTree}.flatMap(${Function(List(parameter), innerTree)})"
          }
        }
      }

      override def tpe = inner.tpe

      override def flatMap(f: Tree => CpsTree): CpsTree = {
        new OpenTree(prefix, parameter, inner.flatMap(f))
      }

    }

    final case class BlockTree(prefix: List[Tree], tail: CpsTree) extends CpsTree {

      override def tpe = tail.tpe

      override def toReflectTree: Tree = {
        Block(prefix, tail.toReflectTree)
      }

      override def flatMap(f: Tree => CpsTree): CpsTree = {
        BlockTree(prefix, tail.flatMap(f))
      }

    }

    final case class MonadTree(reflectTree: Tree, override val tpe: Type) extends CpsTree {

      override def toReflectTree = reflectTree

      override def flatMap(f: Tree => CpsTree): CpsTree = {
        val newId = TermName(c.freshName("element"))
        OpenTree(MonadTree.this, ValDef(Modifiers(PARAM), newId, TypeTree(tpe), EmptyTree), f(Ident(newId)))
      }

    }

    final case class PlainTree(tree: Tree, tpe: Type) extends CpsTree {

      override def toReflectTree: Tree = {
        q"$tree.pure"
      }

      override def flatMap(f: Tree => CpsTree): CpsTree = {
        f(tree)
      }

    }

    private val eachMethodSymbol =
      typeOf[_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.type].member(TermName("each"))
    private val foreachMethodSymbol =
      typeOf[_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.type].member(TermName("foreach"))
    private val mapMethodSymbol =
      typeOf[_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.type].member(TermName("map"))
    private val flatMapMethodSymbol =
      typeOf[_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.type].member(TermName("flatMap"))
    private val filterMethodSymbol =
      typeOf[_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.type].member(TermName("filter"))

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
        case q"""$method[$fType, $a, $u]($fa, $f)""" if method.symbol == foreachMethodSymbol =>
          buildCpsTree(fa).flatMap { faValue =>
            f match {
              case Function(List(valDef), body) =>
                MonadTree(
                  atPos(origin.pos) {
                    q"$faValue.traverse_(${treeCopy.Function(f, List(valDef), buildCpsTree(body).toUntypedTree)})"

                  },
                  origin.tpe
                )
              case _ =>
                buildCpsTree(f).flatMap { fValue =>
                  PlainTree(
                    atPos(origin.pos) {
                      q"$faValue.foreach($fValue)"
                    },
                    origin.tpe
                  )
                }
            }
          }
        case q"""$method[$fType, $a, $b]($fa, $f)""" if method.symbol == mapMethodSymbol =>
          buildCpsTree(fa).flatMap { faValue =>
            f match {
              case Function(List(valDef), body) =>
                MonadTree(
                  atPos(origin.pos) {
                    q"$faValue.traverse(${treeCopy.Function(f, List(valDef), buildCpsTree(body).toUntypedTree)})"

                  },
                  origin.tpe
                )
              case _ =>
                buildCpsTree(f).flatMap { fValue =>
                  PlainTree(atPos(origin.pos)(q"$faValue.map($fValue)"), origin.tpe)
                }
            }
          }
        case q"""$method[$fType, $a, $b]($fa, $f)""" if method.symbol == flatMapMethodSymbol =>
          buildCpsTree(fa).flatMap { faValue =>
            f match {
              case Function(List(valDef), body) =>
                MonadTree(
                  atPos(origin.pos) {
                    q"$faValue.flatTraverse(${treeCopy.Function(f, List(valDef), buildCpsTree(body).toUntypedTree)})"
                  },
                  origin.tpe
                )
              case _ =>
                buildCpsTree(f).flatMap { fValue =>
                  PlainTree(atPos(origin.pos)(q"$faValue.flatMap($fValue)"), origin.tpe)
                }
            }
          }
        case q"""$method[$fType, $a]($fa, $traverse, $monadPlus, $f)""" if method.symbol == filterMethodSymbol =>
          buildCpsTree(fa).flatMap { faValue =>
            f match {
              case Function(List(valDef), body) =>
                MonadTree(
                  atPos(origin.pos) {
                    q"$faValue.filterA(${treeCopy.Function(f, List(valDef), buildCpsTree(body).toUntypedTree)})"
                  },
                  origin.tpe
                )
              case _ =>
                buildCpsTree(f).flatMap { fValue =>
                  PlainTree(atPos(origin.pos)(q"$faValue.filter($fValue)"), origin.tpe)
                }
            }
          }
        case Apply(method @ Ident(name), parameters) if forceAwait(name) => {
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
          val blockTree = buildCpsTree(block).toReflectTree
          val exceptionName = TermName(c.freshName("exception"))

          val tryCatch = {
            q"$blockTree.recoverWith(${Match(
              Ident(exceptionName),
              for (caseTree @ CaseDef(pat, guard, body) <- catches) yield {
                treeCopy.CaseDef(caseTree, pat, guard, buildCpsTree(body).toReflectTree)
              }
            )})"
          }
          if (finalizer.isEmpty) {
            MonadTree(tryCatch, origin.tpe)
          } else {
            val finalizerCpsTree = buildCpsTree(finalizer)
            MonadTree(
              {
                q"$tryCatch.handleErrorWith(${Function(
                  List(ValDef(Modifiers(PARAM), exceptionName, TypeTree(typeOf[_root_.java.lang.Throwable]), EmptyTree)),
                  finalizerCpsTree.flatMap { transformedFinalizer =>
                    if (isDiscardable(finalizerCpsTree)) {
                      MonadTree(q"$exceptionName.raiseError", origin.tpe)
                    } else {
                      MonadTree(q"$transformedFinalizer; $exceptionName.raiseError", origin.tpe)
                    }
                  }.toReflectTree
                )})"
              },
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
                      case transformedTail @ (_: MonadTree | _: OpenTree | _: PlainTree) => {
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
            atPos(origin.pos) {
              q"${buildCpsTree(selector).toReflectTree}.flatMap(${Function(
                List(ValDef(Modifiers(PARAM), selectorName, TypeTree(selector.tpe), EmptyTree)),
                treeCopy.Match(origin, Ident(selectorName), for {
                  cd @ CaseDef(pat, guard, body) <- cases
                } yield treeCopy.CaseDef(cd, pat, guard, buildCpsTree(body).toReflectTree))
              )})"
            },
            origin.tpe
          )
        }
        case If(cond, thenp, elsep) => {
          new MonadTree(
            atPos(origin.pos) {
              q"${buildCpsTree(cond).toReflectTree}.ifM(${buildCpsTree(thenp).toReflectTree},${buildCpsTree(elsep).toReflectTree})"
            },
            origin.tpe
          )
        }
        case Typed(expr, tpt @ Ident(typeNames.WILDCARD_STAR)) => {
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
        case LabelDef(name1,
                      List(),
                      If(condition, block @ Block(body, Apply(Ident(name2), List())), Literal(Constant(()))))
            if name1 == name2 => {
          new MonadTree(
            atPos(origin.pos) {
              q"(${buildCpsTree(treeCopy.Block(block, body, Literal(Constant(())))).toReflectTree}).whileM_(${buildCpsTree(condition).toReflectTree})"
            },
            origin.tpe
          )
        }
        case LabelDef(name1,
                      List(),
                      block @ Block(body, If(condition, Apply(Ident(name2), List()), Literal(Constant(())))))
            if name1 == name2 => {
          new MonadTree(
            atPos(origin.pos) {
              Block(
                List(
                  ValDef(Modifiers(),
                         name1,
                         TypeTree(),
                         buildCpsTree(treeCopy.Block(block, body, Literal(Constant(())))).toReflectTree)), {
                  q"${Ident(name1)}.flatMap(${Function(
                    List(ValDef(Modifiers(PARAM), TermName(c.freshName("ignoredParameter")), TypeTree(definitions.UnitTpe), EmptyTree)),
                    q"${Ident(name1)}.whileM_(${buildCpsTree(condition).toReflectTree})"
                  )})"

                }
              )
            },
            origin.tpe
          )
        }
        case Throw(throwable) => {
          buildCpsTree(throwable).flatMap { x =>
            new MonadTree(atPos(origin.pos) {
              q"$x.raiseError"
            }, origin.tpe)
          }
        }

        case EmptyTree | _: Return | _: New | _: Ident | _: Literal | _: Super | _: This | _: TypTree | _: TypeDef |
            _: Function | _: DefDef | _: ClassDef | _: ModuleDef | _: Import | _: ImportSelector => {
          new PlainTree(origin, origin.tpe)
        }
      }
    }

    def apply[A: WeakTypeTag](body: Tree): Tree = {
      // c.info(c.enclosingPosition, showCode(body), true)
      val result = buildCpsTree(body)(Set.empty).toUntypedTree
      // c.info(c.enclosingPosition, showCode(result), true)
      result
    }

  }

  object Instructions {

    @compileTimeOnly("""`each` instructions must not appear outside monadic blocks.
  Note that the `each` instructions may be renamed for different domains.
  The renamed instruction name may be `bind`, `!`, `await`, `gen`, etc.)""")
    def each[F[_], A](fa: F[A]): A = ???

    @compileTimeOnly("`foreach` instructions must not appear outside monadic blocks")
    def foreach[F[_], A, U](fa: F[A], body: A => U): Unit = ???

    @compileTimeOnly("`map` instructions must not appear outside monadic blocks")
    def map[F[_], A, B](fa: F[A], body: A => B): F[B] = ???

    @compileTimeOnly("`flatMap` instructions must not appear outside monadic blocks")
    def flatMap[F[_], A, B](fa: F[A], body: A => F[B]): F[B] = ???

    @compileTimeOnly("`filter` instructions must not appear outside monadic blocks")
    def filter[F[_], A](fa: F[A], body: A => Boolean): F[A] = ???

  }

}
