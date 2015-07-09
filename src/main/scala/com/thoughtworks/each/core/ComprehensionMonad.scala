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

import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import scalaz.Monad

object ComprehensionMonad {
  def apply[F[_]]: Monad[F] = macro applyImpl

  def applyImpl(c: scala.reflect.macros.whitebox.Context): c.Tree = {
    import c.universe.Flag._
    import c.universe._

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
