package com.thoughtworks.scalazMonadFactory

import scala.language.higherKinds
import scala.language.experimental.macros
import scalaz.Bind

object ComprehensionBind {
  def apply[F[_]]: Bind[F] = macro applyImpl

  def applyImpl(c: scala.reflect.macros.whitebox.Context): c.Tree = {

    import c.universe._
    import c.universe.Flag._
    //    c.info(c.enclosingPosition, showRaw(c.macroApplication), true)

    val TypeApply(_, List(fTypeTree: TypeTree)) = c.macroApplication

    val typeParameter1Name = c.freshName("A")
    val typeParameter2Name = c.freshName("B")
    val fName = c.freshName("f")
    val faName = c.freshName("fa")
    val BindClassName = c.freshName("Bind")

    Block(
      List(
        ClassDef(
          Modifiers(FINAL),
          TypeName(BindClassName),
          List(),
          Template(
            List(
              AppliedTypeTree(Ident(typeOf[_root_.scalaz.Bind[({type T[A] = {}})#T]].typeSymbol), List(fTypeTree))),
            noSelfType,
            List(
              DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))),
              DefDef(
                Modifiers(OVERRIDE),
                TermName("map"),
                List(
                  TypeDef(Modifiers(PARAM), TypeName(typeParameter1Name), List(), TypeBoundsTree(EmptyTree, EmptyTree)),
                  TypeDef(Modifiers(PARAM), TypeName(typeParameter2Name), List(), TypeBoundsTree(EmptyTree, EmptyTree))),
                List(
                  List(
                    ValDef(Modifiers(PARAM), TermName(faName), AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName(typeParameter1Name)))), EmptyTree)),
                  List(
                    ValDef(Modifiers(PARAM), TermName(fName),
                      AppliedTypeTree(
                        Ident(definitions.FunctionClass(1)),
                        List(
                          Ident(TypeName(typeParameter1Name)),
                          Ident(TypeName(typeParameter2Name)))),
                      EmptyTree))),
                AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName(typeParameter2Name)))),
                Apply(Select(Ident(TermName(faName)), TermName("map")), List(Ident(TermName(fName))))),
              DefDef(
                Modifiers(OVERRIDE),
                TermName("bind"),
                List(TypeDef(Modifiers(PARAM), TypeName(typeParameter1Name), List(), TypeBoundsTree(EmptyTree, EmptyTree)), TypeDef(Modifiers(PARAM), TypeName(typeParameter2Name), List(), TypeBoundsTree(EmptyTree, EmptyTree))),
                List(
                  List(
                    ValDef(Modifiers(PARAM), TermName(faName), AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName(typeParameter1Name)))), EmptyTree)),
                  List(
                    ValDef(Modifiers(PARAM), TermName(fName), AppliedTypeTree(Ident(definitions.FunctionClass(1)), List(Ident(TypeName(typeParameter1Name)), AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName(typeParameter2Name)))))), EmptyTree))),
                AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName(typeParameter2Name)))),
                Apply(Select(Ident(TermName(faName)), TermName("flatMap")), List(Ident(TermName(fName))))))))),
      Apply(Select(New(Ident(TypeName(BindClassName))), termNames.CONSTRUCTOR), List()))
  }
}
