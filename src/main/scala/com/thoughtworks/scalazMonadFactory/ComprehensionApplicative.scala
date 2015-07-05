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

    c.info(c.enclosingPosition, showRaw(fTypeTree), true)

    println("companion object of: " + fTypeTree.tpe.typeSymbol.companion)

    Block(
      List(
        ClassDef(
          Modifiers(FINAL),
          TypeName("$anon"),
          List(),
          Template(
            List(AppliedTypeTree(Select(Ident(TermName("scalaz")), TypeName("Applicative")), List(fTypeTree))),
            noSelfType,
            List(
              DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))),
              DefDef(
                Modifiers(OVERRIDE), TermName("point"), List(TypeDef(Modifiers(PARAM), TypeName("A"), List(), TypeBoundsTree(EmptyTree, EmptyTree))),
                List(List(
                  ValDef(
                    Modifiers(PARAM | BYNAMEPARAM | COVARIANT), TermName("a"),
                    AppliedTypeTree(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TypeName("<byname>")), List(Ident(TypeName("A")))),
                    EmptyTree)
                )),
                AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName("A")))),
                Apply(Ident(fTypeTree.tpe.typeSymbol.companion), List(Ident(TermName("a"))))
              ),
              DefDef(
                Modifiers(OVERRIDE), TermName("ap"),
                List(
                  TypeDef(Modifiers(PARAM), TypeName("A"), List(), TypeBoundsTree(EmptyTree, EmptyTree)),
                  TypeDef(Modifiers(PARAM), TypeName("B"), List(), TypeBoundsTree(EmptyTree, EmptyTree))
                ),
                List(
                  List(
                    ValDef(
                      Modifiers(PARAM | BYNAMEPARAM | COVARIANT), TermName("fa"),
                      AppliedTypeTree(
                        Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TypeName("<byname>")),
                        List(AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName("A")))))
                      ),
                      EmptyTree)
                  ),
                  List(
                    ValDef(Modifiers(PARAM | BYNAMEPARAM | COVARIANT), TermName("ff"),
                      AppliedTypeTree(
                        Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TypeName("<byname>")),
                        List(
                          AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol),
                            List(
                              AppliedTypeTree(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TypeName("Function1")),
                                List(Ident(TypeName("A")), Ident(TypeName("B")))))))
                      ), EmptyTree))),
                AppliedTypeTree(Ident(fTypeTree.tpe.typeSymbol), List(Ident(TypeName("B")))),
                Apply(
                  Select(Ident(TermName("fa")), TermName("flatMap")),
                  List(Function(
                    List(ValDef(Modifiers(PARAM), TermName("a"), TypeTree(), EmptyTree)),
                    Apply(
                      Select(Ident(TermName("ff")), TermName("map")),
                      List(Function(
                        List(ValDef(Modifiers(PARAM), TermName("f"), TypeTree(), EmptyTree)),
                        Apply(Ident(TermName("f")), List(Ident(TermName("a"))))))))))))))),
      Apply(Select(New(Ident(TypeName("$anon"))), termNames.CONSTRUCTOR), List()))
  }

}
