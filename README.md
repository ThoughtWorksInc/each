# ThoughtWorks Each <a href="http://thoughtworks.com/"><img align="right" src="https://www.thoughtworks.com/imgs/tw-logo.png" title="ThoughtWorks" height="15"/></a>

[![Join the chat at https://gitter.im/ThoughtWorksInc/each](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/ThoughtWorksInc/each?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/ThoughtWorksInc/each.svg?branch=3.3.x)](https://travis-ci.org/ThoughtWorksInc/each)
[![Latest version](https://index.scala-lang.org/thoughtworksinc/each/each/latest.svg)](https://index.scala-lang.org/thoughtworksinc/each/each)

**ThoughtWorks Each** is a macro library that converts native imperative syntax to [Scalaz](http://scalaz.org/)'s monadic expression. See the [object cats](https://javadoc.io/page/com.thoughtworks.dsl/dsl_2.12/latest/com/thoughtworks/dsl/domains/cats$.html) in [Dsl.scala](https://github.com/ThoughtWorksInc/Dsl.scala/) for the similar feature for [Cats](https://typelevel.org/cats/).

## Motivation

There is a macro library [Stateless Future](https://github.com/qifun/stateless-future) that provides `await` for asynchronous programming.
`await` is a mechanism that transform synchronous-like code into asynchronous expressions. C# 5.0, ECMAScript 7 and Python 3.5 also support the mechanism.

The `await` mechanism in Stateless Future is implemented by an algorithm called [CPS transform](https://en.wikipedia.org/wiki/Continuation-passing_style). When learning [scalaz](https://scalaz.github.io/scalaz/), we found that the same algorithm could be applied for any monadic expression, including `Option` monad, `IO` monad, and `Future` monad. So we started this project, Each.

Each is a superset of `await` syntax. Each supports multiple types of monads, while `await` only works with `Future`. When we perform a CPS transform for monadic expression with the `Future` monad, the use case looks almost the same as the `await` syntax in [Stateless Future](https://github.com/qifun/stateless-future). Each is like F#'s [Computation Expressions](https://msdn.microsoft.com/en-us/library/dd233182.aspx), except Each reuses the normal Scala syntax instead of reinventing new syntax.

For example:

``` scala
import com.thoughtworks.each.Monadic._
import scalaz.std.scalaFuture._

// Returns a Future of the sum of the length of each string in each parameter Future,
// without blocking any thread.
def concat(future1: Future[String], future2: Future[String]): Future[Int] = monadic[Future] {
  future1.each.length + future2.each.length
}
```

The similar code works for monads other than `Future`:

``` scala
import com.thoughtworks.each.Monadic._
import scalaz.std.option._

def plusOne(intOption: Option[Int]) = monadic[Option] {
  intOption.each + 1
}
assert(plusOne(None) == None)
assert(plusOne(Some(15)) == Some(16))
```

``` scala
import com.thoughtworks.each.Monadic._
import scalaz.std.list._

def plusOne(intSeq: List[Int]) = monadic[List] {
  intSeq.each + 1
}
assert(plusOne(Nil) == Nil)
assert(plusOne(List(15)) == List(16))
assert(plusOne(List(15, -2, 9)) == List(16, -1, 10))
```

## Usage

### Step 1: Add the following line in your build.sbt

``` sbt
libraryDependencies += "com.thoughtworks.each" %% "each" % "latest.release"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

or `%%%` for Scala.js projects:

``` sbt
libraryDependencies += "com.thoughtworks.each" %%% "each" % "latest.release"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

Note that ThoughtWorks Each requires Scalaz 7.2.x and does not compatible with Scala 7.1.x .

See https://repo1.maven.org/maven2/com/thoughtworks/each/ for a list of available versions.

### Step 2: In your source file, import `monadic` and `each` method

``` scala
import com.thoughtworks.each.Monadic._
```

### Step 3: Import implicit Monad instances

Scalaz has provided `Option` monad, so you just import it.

``` scala
import com.thoughtworks.each.Monadic._
import scalaz.std.option._
```

Please import other monad instances if you need other monads.

### Step 4: Use `monadic[F]` to create a monadic expression

``` scala
import com.thoughtworks.each.Monadic._
import scalaz.std.option._
val result: Option[String] = monadic[Option] {
  "Hello, Each!"
}
```

### Step 5: In the `monadic` block, use `.each` postfix to extract each element in a `F`

``` scala
import com.thoughtworks.each.Monadic._
import scalaz.std.option._
val name = Option("Each")
val result: Option[String] = monadic[Option] {
  "Hello, " + name.each + "!"
}
```

## Exception handling

`monadic` blocks do not support `try`, `catch` and `finally`. If you want these expressions, use `throwableMonadic` or `catchIoMonadic` instead, for example:

``` scala
var count = 0
val io = catchIoMonadic[IO] {
  count += 1                // Evaluates immediately
  val _ = IO(()).each       // Pauses until io.unsafePerformIO()
  try {
    count += 1
    (null: Array[Int])(0)   // Throws a NullPointerException
  } catch {
    case e: NullPointerException => {
      count += 1
      100
    }
  } finally {
    count += 1
  }
}
assertEquals(1, count)
assertEquals(100, io.unsafePerformIO())
assertEquals(4, count)
```

Note that `catchIoMonadic` requires an implicit parameter `scalaz.effect.MonadCatchIO[F]` instead of `Monad[F]`. `scalaz.effect.MonadCatchIO[F]` is only provided for `scalaz.effect.IO` by default.

## `for` loop

Each supports `.each` magic in a `for` loop on any instances that support `Foldable` type class. For example, you could `import scalaz.std.list._` to enable the `Foldable` type class for  `List`.

``` scala
import com.thoughtworks.each.Monadic._
import scalaz.std.list._
import scalaz.std.option._
val n = Some(10)
@monadic[Option] val result = {
  var count = 1
  for (i <- List(300, 20)) {
    count += i * n.each
  }
  count
}
Assert.assertEquals(Some(3201), result)
```

Note that you need to use `@monadic[Option]` annotation instead of `monadic[Option]` block to in order to enable the `for` loop syntax.

## `for` comprehension

Each also supports `.each` magic in a `for` comprehension on any instances that support `Traverse` and `MonadPlus` type class.

``` scala
import com.thoughtworks.each.Monadic._
import scalaz.std.list._
val n = Some(4000)
@monadic[Option] val result = {
  for {
    i <- List(300, 20)
    (j, k) <- List(50000 -> "1111", 600000 -> "yyy")
    if i > n.each - 3900
    a = i + j
  } yield {
    a + n.each * k.length
  }
}
Assert.assertEquals(Some(List(66300, 612300)), result)
```

Note that you need to use `@monadic[Option]` annotation instead of `monadic[Option]` block to in order to enable the `for` comprehension syntax.

## Limitation

If a [call-by-name parameter](http://www.scala-lang.org/files/archive/spec/2.11/06-expressions.html#function-applications) of a method call is a monadic expression, `Each` will transform the monadic expression before the method call. The behavior was discussed at [#37](https://github.com/ThoughtWorksInc/each/issues/37).

```scala
def innerFailureFuture = Future.failed(new Exception("foo"))
val someValue = Some("value")
val result = monadic[Future] {
  someValue.getOrElse(innerFailureFuture.each)
}
```

`result` will be a future of failure because the above example equals to

```scala
def innerFailureFuture = Future.failed(new Exception("foo"))
val someValue = Some("value")
val result = innerFailureFuture.map(someValue.getOrElse)
```

`innerFailureFuture.each` is evaluated before being passed to `getOrElse` method call, even if `getOrElse` accepts a call-by-name parameter.

## Links

 * [The API Documentation](https://www.javadoc.io/doc/com.thoughtworks.each/each_2.13/latest/com/thoughtworks/each/Monadic$.html)
 * Utilities
   * [ComprehensionMonad](https://github.com/ThoughtWorksInc/each/wiki/ComprehensionMonad)

