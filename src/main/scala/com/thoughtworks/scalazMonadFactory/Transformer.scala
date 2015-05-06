package com.thoughtworks.scalazMonadFactory

import scala.language.experimental.macros
import scala.annotation.compileTimeOnly
import scala.language.implicitConversions
import scala.language.higherKinds

case class Transformer[M[_]]() {

  /**
   * @usecase def async[X](inputTree: X)(implicit bind: scalaz.Bind[M], applicative: scalaz.Applicative[M]): M[X]
   * @usecase def async[X](inputTree: X)(implicit monadCatchIo: scalaz.effect.MonadCatchIO[M]): M[X]
   */
  def async[X](inputTree: X): M[X] = macro Transformer.async_impl

  @compileTimeOnly("`await` must be inside `async`.")
  implicit def await[X](m: M[X]): X = ???
}

object Transformer {

  def async_impl(c: scala.reflect.macros.blackbox.Context)(inputTree: c.Tree): c.Tree = {
    import c.universe._
    import c.universe.Flag._
    c.info(c.enclosingPosition, showRaw(inputTree), true)
    val Apply(TypeApply(Select(thisTree, _), List(asyncValueTypeTree)), _) = c.macroApplication
    val thisType = thisTree.tpe
    val List(monadType) = thisType.widen.typeArgs
    c.info(c.enclosingPosition, show(monadType), true)

    abstract class TransformedTree {
      def monad: c.Tree
      def tpe: Type
      def flatMap(f: c.Tree => TransformedTree): TransformedTree
      def prepend(head: c.Tree) = {
        BlockTree(head :: Nil, this)
      }
    }

    final case class OpenTree(prefix: TransformedTree, parameter: ValDef, inner: TransformedTree) extends TransformedTree { self =>

      override final def monad: c.Tree = {
        inner match {
          case PlainTree(plain, _) => {
            Apply(
              Apply(
                Select(TypeApply(reify(_root_.scalaz.Apply).tree, List(TypeTree(monadType))), TermName("map")),
                List(prefix.monad)),
              List(
                Function(List(parameter), plain)))
          }
          case _ => {
            Apply(
              Apply(
                Select(TypeApply(reify(_root_.scalaz.Bind).tree, List(TypeTree(monadType))), TermName("bind")),
                List(prefix.monad)),
              List(
                Function(List(parameter), inner.monad)))
          }
        }
      }

      override final def tpe = inner.tpe

      override final def flatMap(f: c.Tree => TransformedTree): TransformedTree = {
        new OpenTree(
          prefix = self.prefix,
          parameter = self.parameter,
          inner = self.inner.flatMap(f))
      }

    }

    final case class MonadTree(override final val monad: c.Tree, override final val tpe: Type) extends TransformedTree {

      override final def flatMap(f: c.Tree => TransformedTree): TransformedTree = {
        val newId = TermName(c.freshName("parameter"))
        OpenTree(MonadTree.this, ValDef(Modifiers(PARAM), newId, TypeTree(tpe), EmptyTree), f(Ident(newId)))
      }
    }

    final case class BlockTree(prefix: List[c.Tree], tail: TransformedTree) extends TransformedTree {

      override final def tpe = tail.tpe
      override final def prepend(head: c.Tree) = {
        BlockTree(head :: prefix, tail)
      }

      override final def monad: c.Tree = {
        Block(prefix, tail.monad)
      }
      override final def flatMap(f: c.Tree => TransformedTree): TransformedTree = {
        BlockTree(prefix, tail.flatMap(f))
      }

    }
    final case class PlainTree(tree: Tree, tpe: Type) extends TransformedTree {

      override final def monad: c.Tree = {
        Apply(
          Select(TypeApply(reify(_root_.scalaz.Applicative).tree, List(TypeTree(monadType))), TermName("point")),
          List(tree))
      }

      override final def flatMap(f: c.Tree => TransformedTree): TransformedTree = {
        f(tree)
      }
    }
    def typed(transformedTree: TransformedTree, tpe: Type): TransformedTree = {
      transformedTree.flatMap { x => PlainTree(Typed(x, TypeTree(tpe)), tpe) }
    }

    def transform(origin: c.Tree): TransformedTree = {
      origin match {

        case Apply(
          TypeApply(
            Select(transformer, await),
            List(awaitValueTypeTree)),
          List(monadTree)) if await.decodedName.toString == "await" &&
          transformer.tpe =:= thisType => {
          transform(monadTree).flatMap { x =>
            new MonadTree(x, awaitValueTypeTree.tpe)
          }
        }
        case Try(block, catches, finalizer) => {
          val tryCatch = catches.foldLeft(transform(block).monad) { (tryMonad, cd) =>
            val CaseDef(pat, guard, body) = cd

            val exceptionName = TermName(c.freshName("exception"))
            val catcherResultName = TermName(c.freshName("catcherResult"))

            Apply(
              Apply(
                TypeApply(
                  Select(reify(_root_.scalaz.effect.MonadCatchIO).tree, TermName("catchSome")),
                  List(
                    Ident(monadType.typeSymbol),
                    TypeTree(origin.tpe),
                    AppliedTypeTree(Ident(monadType.typeSymbol), List(TypeTree(origin.tpe))))),
                List(tryMonad)),
              List(
                Function(
                  List(ValDef(Modifiers(PARAM), exceptionName, TypeTree(typeOf[_root_.java.lang.Throwable]), EmptyTree)),
                  Match(
                    Ident(exceptionName),
                    List(
                      treeCopy.CaseDef(cd, pat, guard, Apply(reify(_root_.scala.Some).tree, List(transform(body).monad))),
                      CaseDef(Ident(termNames.WILDCARD), EmptyTree, reify(_root_.scala.None).tree)))),
                Function(
                  List(
                    ValDef(
                      Modifiers(PARAM),
                      catcherResultName,
                      AppliedTypeTree(Ident(monadType.typeSymbol), List(TypeTree(origin.tpe))),
                      EmptyTree)),
                  Ident(catcherResultName))))

          }
          if (finalizer.isEmpty) {
            MonadTree(tryCatch, origin.tpe)
          } else {
            MonadTree(Apply(Select(reify(_root_.scalaz.effect.MonadCatchIO).tree, TermName("ensuring")), List(tryCatch, transform(finalizer).monad)), origin.tpe)
          }
        }
        case ClassDef(mods, _, _, _) => {
          ???
        }
        case _: ModuleDef => {
          ???
        }
        case DefDef(mods, _, _, _, _, _) => {
          ???
        }
        case _: Function => {
          ???
        }
        case Select(instance, field) => {
          transform(instance).flatMap { x =>
            new PlainTree(Select(x, field), origin.tpe)
          }
        }
        case TypeApply(method, parameters) => {
          transform(method).flatMap { x =>
            new PlainTree(TypeApply(x, parameters), origin.tpe)
          }
        }
        case Apply(method, parameters) => {
          def transformParameters(untransformed: List[Tree], transformed: List[Tree]): TransformedTree = {
            untransformed match {
              case Nil => {
                transform(method).flatMap { transformedMethod =>
                  new PlainTree(Apply(transformedMethod, transformed.reverse), origin.tpe)
                }
              }
              case head :: tail => {
                transform(head).flatMap { transformedHead =>
                  transformParameters(tail, transformedHead :: transformed)
                }
              }
            }
          }
          transformParameters(parameters, Nil)
        }
        case Block(stats, expr) => {

          def transformStats(untransformed: List[c.Tree]): TransformedTree = {
            untransformed match {
              case Nil => {
                transform(expr)
              }
              case head :: tail => {
                transform(head).flatMap { transformedHead =>
                  transformStats(tail).prepend(transformedHead)
                }
              }
            }
          }
          new MonadTree(transformStats(stats).monad, origin.tpe)

        }
        case ValDef(mods, name, tpt, rhs) => {
          transform(rhs).flatMap { x =>
            new PlainTree(treeCopy.ValDef(origin, mods, name, tpt, x), origin.tpe)
          }
        }
        case Assign(left, right) => {
          ???
        }
        case Match(selector, cases) => {
          ???
        }
        case If(cond, thenp, elsep) => {
          new MonadTree(
            Apply(
              TypeApply(
                Select(TypeApply(reify(_root_.scalaz.Bind).tree, List(TypeTree(monadType))), TermName("ifM")),
                List(TypeTree(origin.tpe))),
              List(
                transform(cond).monad,
                transform(thenp).monad,
                transform(elsep).monad)),
            origin.tpe)
        }
        case Throw(throwable) => {
          ???
        }
        case Typed(expr, tpt) => {
          transform(expr).flatMap { x =>
            new PlainTree(Typed(x, tpt), origin.tpe)
          }
        }
        case Annotated(annot, arg) => {
          ???
        }
        case LabelDef(name, params, rhs) => {
          ???
        }
        case _: Return => {
          ???
        }
        case EmptyTree | _: New | _: Ident | _: Literal | _: Super | _: This | _: TypTree | _: New | _: TypeDef | _: Import | _: ImportSelector => {
          new PlainTree(origin, origin.tpe)

        }
      }
    }
    val result = transform(inputTree)
    c.info(c.enclosingPosition, show(result.monad), true)
    c.untypecheck(result.monad)
  }

}