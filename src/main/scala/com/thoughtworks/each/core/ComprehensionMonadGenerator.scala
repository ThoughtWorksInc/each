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

object ComprehensionMonadGenerator {

  def generatorMonad[U <: scala.reflect.api.Universe](u: U, freshName: String => String)(fSymbol: u.TypeSymbol): u.Tree = {
    import u._
    import u.Flag._

    val typeParameter1Name = freshName("A")
    val typeParameter2Name = freshName("B")
    val aName = freshName("a")
    val ffName = freshName("ff")
    val fName = freshName("f")
    val faName = freshName("fa")
    val monadClassName = freshName("Monad")
    Block(
      List(
        ClassDef(
          Modifiers(FINAL),
          TypeName(monadClassName),
          List(),
          Template(
            List(AppliedTypeTree(Ident(typeOf[_root_.scalaz.Monad[({type T[A] = {}})#T]].typeSymbol), List(Ident(fSymbol)))),
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
                  List(ValDef(Modifiers(PARAM), TermName(faName), AppliedTypeTree(Ident(fSymbol), List(Ident(TypeName(typeParameter1Name)))), EmptyTree)),
                  List(
                    ValDef(
                      Modifiers(PARAM),
                      TermName(fName),
                      AppliedTypeTree(
                        Ident(definitions.FunctionClass(1)),
                        List(Ident(TypeName(typeParameter1Name)), AppliedTypeTree(Ident(fSymbol), List(Ident(TypeName(typeParameter2Name)))))
                      ),
                      EmptyTree)
                  )
                ),
                AppliedTypeTree(Ident(fSymbol), List(Ident(TypeName(typeParameter2Name)))),
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
                AppliedTypeTree(Ident(fSymbol), List(Ident(TypeName(typeParameter1Name)))),
                Apply(Ident(fSymbol.companion), List(Ident(TermName(aName))))),
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
                        List(AppliedTypeTree(Ident(fSymbol), List(Ident(TypeName(typeParameter1Name)))))
                      ),
                      EmptyTree)
                  ),
                  List(
                    ValDef(Modifiers(PARAM | BYNAMEPARAM | COVARIANT), TermName(ffName),
                      AppliedTypeTree(
                        Ident(definitions.ByNameParamClass),
                        List(
                          AppliedTypeTree(Ident(fSymbol),
                            List(
                              AppliedTypeTree(Ident(definitions.FunctionClass(1)),
                                List(Ident(TypeName(typeParameter1Name)), Ident(TypeName(typeParameter2Name)))))))
                      ), EmptyTree))),
                AppliedTypeTree(Ident(fSymbol), List(Ident(TypeName(typeParameter2Name)))),
                Apply(
                  Select(Ident(TermName(faName)), TermName("flatMap")),
                  List(Function(
                    List(ValDef(Modifiers(PARAM), TermName(aName), TypeTree(), EmptyTree)),
                    Apply(
                      Select(Ident(TermName(ffName)), TermName("map")),
                      List(Function(
                        List(ValDef(Modifiers(PARAM), TermName(fName), TypeTree(), EmptyTree)),
                        Apply(Ident(TermName(fName)), List(Ident(TermName(aName))))))))))),
              DefDef(
                Modifiers(OVERRIDE),
                TermName("map"),
                List(
                  TypeDef(Modifiers(PARAM), TypeName(typeParameter1Name), List(), TypeBoundsTree(EmptyTree, EmptyTree)),
                  TypeDef(Modifiers(PARAM), TypeName(typeParameter2Name), List(), TypeBoundsTree(EmptyTree, EmptyTree))),
                List(
                  List(
                    ValDef(Modifiers(PARAM), TermName(faName), AppliedTypeTree(Ident(fSymbol), List(Ident(TypeName(typeParameter1Name)))), EmptyTree)),
                  List(
                    ValDef(Modifiers(PARAM), TermName(fName),
                      AppliedTypeTree(
                        Ident(definitions.FunctionClass(1)),
                        List(
                          Ident(TypeName(typeParameter1Name)),
                          Ident(TypeName(typeParameter2Name)))),
                      EmptyTree))),
                AppliedTypeTree(Ident(fSymbol), List(Ident(TypeName(typeParameter2Name)))),
                Apply(Select(Ident(TermName(faName)), TermName("map")), List(Ident(TermName(fName)))))
            )
          )

        )
      ),
      Apply(Select(New(Ident(TypeName(monadClassName))), termNames.CONSTRUCTOR), List()))

  }
}
