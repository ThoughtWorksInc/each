# Each

[![Join the chat at https://gitter.im/ThoughtWorksInc/each](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/ThoughtWorksInc/each?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/ThoughtWorksInc/each.svg?branch=master)](https://travis-ci.org/ThoughtWorksInc/each)
[![Maven Central](https://img.shields.io/maven-central/v/com.thoughtworks.each/each_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/com.thoughtworks.each/each_2.11)
[![Codacy Badge](https://www.codacy.com/project/badge/3ed3f896c735432ca8e9f3963b8cd144)](https://www.codacy.com/app/pop-atry/each)

**Each** is a macro library that converts native imperative syntax to scalaz's monadic expression.

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
assertEquals(None, plusOne(None))
assertEquals(Some(16), plusOne(Some(15)))
```

``` scala
import com.thoughtworks.each.Monadic._
import scalaz.std.list._

def plusOne(intSeq: List[Int]) = monadic[List] {
  intSeq.each + 1
}
assertEquals(Nil, plusOne(Nil))
assertEquals(List(16), plusOne(List(15)))
assertEquals(List(16, -1, 10), plusOne(List(15, -2, 9)))
```

## Usage

### Step 1: Add the following line in your build.sbt

``` sbt
libraryDependencies += "com.thoughtworks.each" %% "each" % "0.4.2"
```

or `%%%` for Scala.js projects:

``` sbt
libraryDependencies += "com.thoughtworks.each" %%% "each" % "0.4.2"
```

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

Each supports `.each` magic in a `for` loop on `MonadicLoop`. You can create a `MonadicLoop` instance via `.monadicLoop` method from any instances that support `Foldable` type class. For example, you could `import scalaz.std.list._` to enable the `Foldable` type class for  `List`.

``` scala
import com.thoughtworks.each.Monadic._
import scalaz.std.list._
import scalaz.std.option._
val n = Some(10)
val result = monadic[Option] {
  var count = 1
  for (i <- List(300, 20).monadicLoop) {
    count += i * n.each
  }
  count
}
Assert.assertEquals(Some(3201), result)
```

## `for` comprehension

Each also supports `.each` magic in a `for` comprehension on `MonadicLoop`. You can create a `MonadicLoop` instance via `.monadicLoop` method from any instances that support `Traverse` and `MonadPlus` type class.

``` scala
import com.thoughtworks.each.Monadic._
import scalaz.std.list._
val n = Some(4000)
val result = monadic[Option] {
  (for {
    i <- List(300, 20).monadicLoop
    (j, k) <- List(50000 -> "1111", 600000 -> "yyy").monadicLoop
    if i > n.each - 3900
    a = i + j
  } yield {
    a + n.each * k.length
  }).underlying
}
Assert.assertEquals(Some(List(66300, 612300)), result)
```


## Links

 * [The API Documentation](https://oss.sonatype.org/service/local/repositories/releases/archive/com/thoughtworks/each/each_2.11/0.4.2/each_2.11-0.4.2-javadoc.jar/!/index.html)
 * Utilities
   * [ComprehensionMonad](https://github.com/ThoughtWorksInc/each/wiki/ComprehensionMonad)

