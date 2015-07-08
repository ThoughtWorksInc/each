package com.thoughtworks.scalazMonadFactory

import scala.language.experimental.macros
import scala.annotation.{tailrec, compileTimeOnly}
import scala.language.implicitConversions
import scala.language.higherKinds
import scalaz.Monad

/**
 * Created by longyang.zhang on 7/8/15.
 */
object ComprehensionMonad {
  def apply[F[_]]: Monad[F] = macro applyImpl

  def applyImpl(c: scala.reflect.macros.whitebox.Context): c.Tree = {
    import c.universe._
    import c.universe.Flag._

    val TypeApply(_, List(fTypeTree: TypeTree)) = c.macroApplication

    val typeParameter1Name = c.freshName("A")
    val typeParameter2Name = c.freshName("B")
    val aName = c.freshName("a")
    val fName = c.freshName("f")
    val faName = c.freshName("fa")
    val monadClassName = c.freshName("Monad")

    val ast = Block(
      List(
        ClassDef(
          Modifiers(FINAL),
          TypeName(monadClassName),
          List(),
          Template(
            List(AppliedTypeTree(Ident(typeOf[_root_.scalaz.Monad[({type T[A] = {}})#T]].typeSymbol), List(fTypeTree))),
            noSelfType,
            List(
              DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))),
              DefDef(
                Modifiers(OVERRIDE),
                TermName("bind"),
                List(
                  TypeDef(Modifiers(PARAM), TypeName(typeParameter1Name), List(), TypeBoundsTree(EmptyTree, EmptyTree)),
                  TypeDef(Modifiers(PARAM), TypeName(typeParameter2Name), List(), TypeBoundsTree(EmptyTree, EmptyTree))
                ),
                List(
                  List(ValDef(Modifiers(PARAM), TermName(faName), AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol),
                    List(Ident(TypeName(typeParameter1Name)))), EmptyTree)
                  ),
                  List(
                    ValDef(
                      Modifiers(PARAM),
                      TermName(fName),
                      AppliedTypeTree(
                        Ident(definitions.FunctionClass(1)),
                        List(Ident(TypeName(typeParameter1Name)), AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName(typeParameter2Name)))))
                      ),
                      EmptyTree)
                  )
                ),
                AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName(typeParameter2Name)))),
                Apply(Select(Ident(TermName(faName)), TermName("flatMap")), List(Ident(TermName(fName))))
              ),
              DefDef(
                Modifiers(OVERRIDE),
                TermName("point"),
                List(TypeDef(Modifiers(PARAM), TypeName(typeParameter1Name), List(), TypeBoundsTree(EmptyTree, EmptyTree))),
                List(
                  List(
                    ValDef(
                      Modifiers(PARAM | BYNAMEPARAM | COVARIANT),
                      TermName(aName),
                      AppliedTypeTree(
                        Ident(definitions.ByNameParamClass),
                        List(Ident(TypeName(typeParameter1Name)))), EmptyTree)
                  )
                ),
                AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName(typeParameter1Name)))),
                Apply(Ident(fTypeTree.tpe.typeSymbol.companion), List(Ident(TermName(aName))))
              )
            )
          )
        )
      ),
      Apply(Select(New(Ident(TypeName(monadClassName))), termNames.CONSTRUCTOR), List())
    )

//    c.info(c.enclosingPosition, show(ast), true)

    ast
  }
}
