package com.thoughtworks.scalazMonadFactory

import scala.language.experimental.macros
import scala.annotation.{tailrec, compileTimeOnly}
import scala.language.implicitConversions
import scala.language.higherKinds
import scalaz.Applicative

object ComprehensionApplicative {

  def apply[F[_]]: Applicative[F] = macro applyImpl

  def applyImpl(c: scala.reflect.macros.whitebox.Context): c.Tree = {
    import c.universe._
    import c.universe.Flag._
    //    c.info(c.enclosingPosition, showRaw(c.macroApplication), true)

    val TypeApply(_, List(fTypeTree: TypeTree)) = c.macroApplication

    //    c.info(c.enclosingPosition, showRaw(fTypeTree), true)

    val typeParameter1Name = c.freshName("A")
    val typeParameter2Name = c.freshName("B")
    val aName = c.freshName("a")
    val fName = c.freshName("f")
    val faName = c.freshName("fa")
    val ffName = c.freshName("ff")
    val applicativeClassName = c.freshName("Applicative")
    
    Block(
      List(
        ClassDef(
          Modifiers(FINAL),
          TypeName(applicativeClassName),
          List(),
          Template(
            List(AppliedTypeTree(Ident(typeOf[_root_.scalaz.Applicative[({type T[A] = {}})#T]].typeSymbol), List(fTypeTree))),
            noSelfType,
            List(
              DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))),
              DefDef(
                Modifiers(OVERRIDE), TermName("point"), List(TypeDef(Modifiers(PARAM), TypeName(typeParameter1Name), List(), TypeBoundsTree(EmptyTree, EmptyTree))),
                List(List(
                  ValDef(
                    Modifiers(PARAM | BYNAMEPARAM | COVARIANT), TermName(aName),
                    AppliedTypeTree(Ident(definitions.ByNameParamClass), List(Ident(TypeName(typeParameter1Name)))),
                    EmptyTree)
                )),
                AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName(typeParameter1Name)))),
                Apply(Ident(fTypeTree.tpe.typeSymbol.companion), List(Ident(TermName(aName))))
              ),
              DefDef(
                Modifiers(OVERRIDE), TermName("ap"),
                List(
                  TypeDef(Modifiers(PARAM), TypeName(typeParameter1Name), List(), TypeBoundsTree(EmptyTree, EmptyTree)),
                  TypeDef(Modifiers(PARAM), TypeName(typeParameter2Name), List(), TypeBoundsTree(EmptyTree, EmptyTree))
                ),
                List(
                  List(
                    ValDef(
                      Modifiers(PARAM | BYNAMEPARAM | COVARIANT), TermName(faName),
                      AppliedTypeTree(
                        Ident(definitions.ByNameParamClass),
                        List(AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName(typeParameter1Name)))))
                      ),
                      EmptyTree)
                  ),
                  List(
                    ValDef(Modifiers(PARAM | BYNAMEPARAM | COVARIANT), TermName(ffName),
                      AppliedTypeTree(
                        Ident(definitions.ByNameParamClass),
                        List(
                          AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol),
                            List(
                              AppliedTypeTree(Ident(definitions.FunctionClass(1)),
                                List(Ident(TypeName(typeParameter1Name)), Ident(TypeName(typeParameter2Name)))))))
                      ), EmptyTree))),
                AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName(typeParameter2Name)))),
                Apply(
                  Select(Ident(TermName(faName)), TermName("flatMap")),
                  List(Function(
                    List(ValDef(Modifiers(PARAM), TermName(aName), TypeTree(), EmptyTree)),
                    Apply(
                      Select(Ident(TermName(ffName)), TermName("map")),
                      List(Function(
                        List(ValDef(Modifiers(PARAM), TermName(fName), TypeTree(), EmptyTree)),
                        Apply(Ident(TermName(fName)), List(Ident(TermName(aName))))))))))))))),
      Apply(Select(New(Ident(TypeName(applicativeClassName))), termNames.CONSTRUCTOR), List()))
  }

}
