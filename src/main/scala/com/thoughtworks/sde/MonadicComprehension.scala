/*
Copyright 2016 ThoughtWorks, Inc.

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

package com.thoughtworks.sde

import scala.annotation.compileTimeOnly
import scalaz.{MonadPlus, Bind, Traverse, Foldable}
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object MonadicComprehension {

  @compileTimeOnly("`foreach` must be inside a SDE block.")
  def foreach[F[_], A, U](fa: F[A])(f: A => U)(implicit foldalbe: Foldable[F]): F[Unit] = ???

  @compileTimeOnly("`map` must be inside a SDE block.")
  def map[F[_] : Traverse, A, B](fa: F[A])(f: A => B): F[B] = ???

  @compileTimeOnly("`flatMap` must be inside a SDE block.")
  def flatMap[F[_], A, B](fa: F[A])(f: A => F[B])(implicit traverse: Traverse[F], bind: Bind[F]): F[B] = ???

  @compileTimeOnly("`withFilter` must be inside a SDE block.")
  def filter[F[_], A](fa: F[A])(f: A => Boolean)(implicit traverse: Traverse[F], monadPlus: MonadPlus[F]): F[A] = ???

}
