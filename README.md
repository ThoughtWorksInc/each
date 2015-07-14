# Each
[![Build Status](https://travis-ci.org/ThoughtWorksInc/each.svg?branch=master)](https://travis-ci.org/ThoughtWorksInc/each)
[![Maven Central](https://img.shields.io/maven-central/v/com.thoughtworks.each/each_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/com.thoughtworks.each/each_2.11)

**Each** is a macro library that converts native imperative syntax to scalaz's monadic expression.

## Motivation

There is a macro library [Stateless Future](https://github.com/qifun/stateless-future) that provides `await` for asynchronous programming.
`await` is a mechanism that transform synchronous-like code into asynchronous expressions. C# 4.5, ECMAScript 7 and Python 3.5 also support the mechanism.

The `await` mechanism in Stateless Future is implemented by an algorithm called [CPS transform](https://en.wikipedia.org/wiki/Continuation-passing_style). When learning [scalaz](https://scalaz.github.io/scalaz/), we found that the same algorithm could be applied for any monadic expression, including `Option` monad, `IO` monad, and `Future` monad. So we started this project, Each.

Each is a superset of `await` syntax. Each supports multiple types of monads, while `await` only works with `Future`. When we perform a CPS transform for monadic expression with the `Future` monad, the use case looks almost the same with the `await` syntax in [Stateless Future](https://github.com/qifun/stateless-future)

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

### Step 1: And add the following line in your build.sbt

``` sbt
libraryDependencies += "com.thoughtworks.each" %% "each" % "0.2.0"
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

`monadic` blocks do not support `try`, `catch` and `finally`. If you want these expressions, use `catchIoMonadic` instead, for example:

``` scala
var count = 0
val io = catchIoMonadic[IO] {
  val _ = IO(()).each
  try {
    count += 1
    (null: Array[Int])(0) // Will throw a NullPointerException
  } catch {
    case e: NullPointerException => {
      count += 1
      100
    }
  } finally {
    count += 1
  }
}
assertEquals(0, count)
assertEquals(100, io.unsafePerformIO())
assertEquals(3, count)
```

Note that `catchIoMonadic` requires an implicit parameter `scalaz.effect.MonadCatchIO[F]` instead of `Monad[F]`. `scalaz.effect.MonadCatchIO[F]` is only provided for `scalaz.effect.IO` by default.

## Links

 * [The API Documentation](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/thoughtworks/each/each_2.11/0.2.1-SNAPSHOT/each_2.11-0.2.1-SNAPSHOT-javadoc.jar/!/index.html)
 * [ComprehensionMonad](https://github.com/ThoughtWorksInc/each/wiki/ComprehensionMonad)

