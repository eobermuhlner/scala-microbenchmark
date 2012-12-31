package ch.obermuhlner.scala.microbenchmark.example
import ch.obermuhlner.scala.microbenchmark.Benchmark
import ch.obermuhlner.scala.microbenchmark.Suite
import ch.obermuhlner.scala.microbenchmark.FunBenchmarks
import ch.obermuhlner.scala.microbenchmark.HtmlReport

import scala.collection.mutable.ListBuffer

import java.util.HashMap
import java.util.concurrent.ConcurrentHashMap

object HtmlExample2 extends HtmlReport {
  def main(args: Array[String]) {

    val benchmarkAll = false
    val longRun = false
    reportDetails = true

    val benchmarkCalibrate = false || benchmarkAll
    val benchmarkScala = false || benchmarkAll
    val benchmarkCalls = false || benchmarkAll
    val benchmarkList = true || benchmarkAll
    val benchmarkSet = false || benchmarkAll
    val benchmarkMap = false || benchmarkAll
    val benchmarkPattern = false || benchmarkAll
    val benchmarkJava = false || benchmarkAll
    val benchmarkLong = false || benchmarkAll
    val benchmarkCurrentTest = true || benchmarkAll

    runner.warmupCount = 1100
    runner.runCount = if (longRun) 1000 else 100
    runner.runLoopCount = if (longRun) 100 else 10
    runner.runTimeThreshold = 1000

    if (benchmarkCalibrate) add("Sleeps",
      "Sleeps for a specified time. Useful to verify the micro-benchmark framework.",
      "Sleep Millis",
      0 to 0,
      0 to 100 by 10,
      new FunBenchmarks[Int, Int] {
        override val runLoopCount = 1
        prepare {
          millis => millis
        }
        run("sleep", "Sleeps for the specified amount of milliseconds.") { millis =>
          Thread.sleep(millis)
        }
      })

    if (benchmarkCalibrate || benchmarkScala) add("Loops",
      "This benchmark suite shows whether the compiler optimizes simple loops. If any of the benchmarks does not show linear cost, then the compiler optimizes it. This will probably impact other benchmarks as well.",
      "Loop Count",
      0 to 2,
      0 to 10000 by 1000,
      new FunBenchmarks[Int, Int] {
        override val runLoopCount = 10
        prepare {
          count => count
        }
        run("for loop", "An empty for loop.") { count =>
          for (i <- 0 to count) {
          }
        }
        run("for loop result", "A for loop that accumulates a result in a variable.") { count =>
          var result = 0
          for (i <- 0 to count) {
            result += i
          }
          result
        }
        run("while loop", "A while loop that counts in a variable without returning a result.") { count =>
          var i = 0
          while (i < count) {
            i += 1
          }
        }
        run("while loop result", "A while loop that accumulates a result in a variable.") { count =>
          var result = 0
          var i = 0
          while (i < count) {
            result += i
            i += 1
          }
          result
        }
        run("do while loop", "A do-while loop that counts in a variable without returning a result.") { count =>
          var i = 0
          do {
            i += 1
          } while (i <= count)
        }
        run("do while loop result", "A do-while loop that accumulates a result in a variable.") { count =>
          var result = 0
          var i = 0
          do {
            result += i
            i += 1
          } while (i <= count)
          result
        }
      })

    if (benchmarkCalibrate || benchmarkScala) add("Arithmetic Int",
      "Integer arithmetic operations.",
      "Loop Count",
      0 to 2,
      0 to 10000 by 1000,
      new FunBenchmarks[Int, Int] {
        val zero = 0
        val value = 7

        override val runLoopCount = 10
        prepare {
          count => count
        }
        run("for loop", "A for loop without calculations. This is the reference for the other benchmarks in this suite.") { count =>
          var result = zero
          for (i <- 0 to count) {
            result = value
          }
          result
        }
        run("Int plus", "Integer addition.") { count =>
          val value = 123
          var result = 0
          for (i <- 0 to count) {
            result += value
          }
          result
        }
        run("Int minus", "Integer subtraction.") { count =>
          val value = 123
          var result = 0
          for (i <- 0 to count) {
            result -= value
          }
          result
        }
        run("Int mult", "Integer multiplication.") { count =>
          var result = value
          for (i <- 0 to count) {
            result *= value
          }
          result
        }
        run("Int div", "Integer division.") { count =>
          var result = value
          for (i <- 0 to count) {
            result /= value
          }
          result
        }
      })

    if (benchmarkScala) add("Arithmetic Double",
      "Integer arithmetic operations.",
      "Loop Count",
      0 to 2,
      0 to 10000 by 1000,
      new FunBenchmarks[Int, Int] {
        val zero = 0
        val value = 7

        override val runLoopCount = 10
        prepare {
          count => count
        }
        run("for loop", "A for loop without calculations. This is the reference for the other benchmarks in this suite.") { count =>
          var result = zero
          for (i <- 0 to count) {
            result = value
          }
          result
        }
        run("Double plus", "Double addition.") { count =>
          var result = zero
          for (i <- 0 to count) {
            result += value
          }
          result
        }
        run("Double minus", "Double subtraction.") { count =>
          var result = zero
          for (i <- 0 to count) {
            result -= value
          }
          result
        }
        run("Double mult", "Double multiplication.") { count =>
          var result = value
          for (i <- 0 to count) {
            result *= value
          }
          result
        }
        run("Double div", "Double division.") { count =>
          var result = value
          for (i <- 0 to count) {
            result /= value
          }
          result
        }
      })

    if (benchmarkScala) add("Arithmetic BigDecimal64",
      "Arithmetic operations using a 64 bit math context (16 significant digits).",
      "Loop Count",
      0 to 2,
      0 to 100 by 10,
      new FunBenchmarks[Int, Int] {
        val mc = java.math.MathContext.DECIMAL64

        val zero: BigDecimal = BigDecimal(0.0, mc)
        val value: BigDecimal = BigDecimal(1, mc) / 7

        override val runLoopCount = 10
        prepare {
          count => count
        }
        run("for loop", "A for loop without calculations. This is the reference for the other benchmarks in this suite.") { count =>
          var result = zero
          for (i <- 0 to count) {
            result = value
          }
          result
        }
        run("BigDecimal plus", "BigDecimal addition.") { count =>
          var result = zero
          for (i <- 0 to count) {
            result += value
          }
          result
        }
        run("BigDecimal minus", "BigDecimal subtraction.") { count =>
          var result = zero
          for (i <- 0 to count) {
            result -= value
          }
          result
        }
        run("BigDecimal mult", "BigDecimal multiplication.") { count =>
          var result = value
          for (i <- 0 to count) {
            result *= value
          }
          result
        }
        run("BigDecimal div", "BigDecimal division.") { count =>
          var result = value
          for (i <- 0 to count) {
            result /= value
          }
          result
        }
      })

    if (benchmarkScala) add("Arithmetic BigDecimal128",
      "Arithmetic operations using a 128 bit math context (34 significant digits).",
      "Loop Count",
      0 to 2,
      0 to 100 by 10,
      new FunBenchmarks[Int, Int] {
        val mc = java.math.MathContext.DECIMAL128

        val zero: BigDecimal = BigDecimal(0.0, mc)
        val value: BigDecimal = BigDecimal(1, mc) / 7

        override val runLoopCount = 10
        prepare {
          count => count
        }
        run("for loop", "A for loop without calculations. This is the reference for the other benchmarks in this suite.") { count =>
          var result = zero
          for (i <- 0 to count) {
            result = value
          }
          result
        }
        run("BigDecimal plus", "BigDecimal addition.") { count =>
          var result = zero
          for (i <- 0 to count) {
            result += value
          }
          result
        }
        run("BigDecimal minus", "BigDecimal subtraction.") { count =>
          var result = zero
          for (i <- 0 to count) {
            result -= value
          }
          result
        }
        run("BigDecimal mult", "BigDecimal multiplication.") { count =>
          var result = value
          for (i <- 0 to count) {
            result *= value
          }
          result
        }
        run("BigDecimal div", "BigDecimal division.") { count =>
          var result = value
          for (i <- 0 to count) {
            result /= value
          }
          result
        }
      })

    if (benchmarkScala) add("Casts",
      "Compares the different ways casts can be implemented in Scala.",
      "Loop Count",
      0 to 2,
      0 to 100000 by 10000,
      new FunBenchmarks[Int, Int] {
        var any: Any = "Hello"
        var result: String = _

        prepare {
          count => count
        }
        run("asInstanceOf", "Casts a value using asInstanceOf.") { count =>
          for (i <- 0 to count) {
            result = any.asInstanceOf[String]
          }
        }
        run("match case", "Casts a value using pattern matching with the type.") { count =>
          for (i <- 0 to count) {
            result = any match {
              case s: String => s
              case _ => throw new IllegalArgumentException
            }
          }
        }
      })

    if (benchmarkScala || benchmarkCalls) add("Calls",
      "Compares the different type of calls in Scala. All calls will return the double integer value of its single integer argument. Example: def anotherMethod(x: Int) = x + x",
      "Loop Count",
      0 to 2,
      0 to 100000 by 10000,
      new FunBenchmarks[Int, Int] {
        val function = (x: Int) => x + x
        val traitInstance = new ExampleTrait() {
          def traitMethod(x: Int) = {
            x + x
          }
        }

        prepare {
          count => count
        }
        run("inlined loop", "A loop with the calculating code inlined.") { count =>
          var result = 0
          for (i <- 0 to count) {
            result += i + i
          }
          result
        }
        run("method", "Calls another method in the same class.") { count =>
          var result = 0
          for (i <- 0 to count) {
            result += anotherMethod(i)
          }
          result
        }
        run("singletonMethod", "Calls a method in a singleton object. This corresponds to a static method in Java.") { count =>
          var result = 0
          for (i <- 0 to count) {
            result += ExampleSingleton.singletonMethod(i)
          }
          result
        }
        run("traitSingletonMethod", "Calls a method declared in a trait and implemented in a singleton object.") { count =>
          var result = 0
          for (i <- 0 to count) {
            result += ExampleTraitSingleton.traitMethod(i)
          }
          result
        }
        run("traitMethodImpl", "Calls a method implemented in a trait.") { count =>
          var result = 0
          for (i <- 0 to count) {
            result += ExampleTraitSingleton.traitMethodImpl(i)
          }
          result
        }
        run("traitInstanceMethod", "Calls a method declared in a trait and implemented in an anonymous local instance.") { count =>
          var result = 0
          for (i <- 0 to count) {
            result += traitInstance.traitMethod(i)
          }
          result
        }
        run("function", "Calls a function.") { count =>
          var result = 0
          for (i <- 0 to count) {
            result += function(i)
          }
          result
        }
        run("closure", "Calls a closure with a bound variable.") { count =>
          var result = 0
          for (i <- 0 to count) {
            val closure = { () => i + i }
            result += closure()
          }
          result
        }

        private def anotherMethod(x: Int) = x + x
      })

    if (benchmarkList || benchmarkCurrentTest) add("List",
      "List methods that operate on a single element.",
      "N elements",
      0 to 2,
      0 to 1000 by 100,
      new FunBenchmarks[Int, List[Int]] {
        prepare {
          length => TestData.list(length)
        }
        run("concatRight", "Concatenates two lists, where the left list has a single element.") { list =>
          List(1) ::: list
        }
        run("appendHead", "Appends a single element at the head of a list.") { list =>
          1 :: list
        }
      })

    if (benchmarkList) add("List 2",
      "List methods that operate on all elements.",
      "N elements",
      0 to 2,
      0 to 1000 by 100,
      new FunBenchmarks[Int, List[Int]] {
        prepare {
          length => TestData.list(length)
        }
        run("sort", "Sorts a random list.") { list =>
          list.sorted
        }
        run("reverse", "Reverses a list.") { list =>
          list.reverse
        }
        run("length", "Counts the elements in a list.") { list =>
          list.length
        }
        run("concatLeft", "Concatenates two lists, where the right list has a single element") { list =>
          list ::: List(1)
        }
        run("appendTail", "Appends a single element at the tail end of a list.") { list =>
          list :: 1 :: Nil
        }
      })

    if (benchmarkMap) add("Immutable Map",
      "Immutable Map methods that operate on a single element.",
      "N elements",
      0 to 10,
      0 to 30,
      new FunBenchmarks[Int, Map[Int, String]] {
        prepare {
          length => TestData.map(length)
        }
        run("contains true", "Checks that a map really contains a value.") { map =>
          map.contains(0)
        }
        run("contains false", "Checks that a map really does not contain a value.") { map =>
          map.contains(-999)
        }
        run("+", "Adds a new entry to a map.") { map =>
          map + (-1 -> "X-1")
        }
        run("-", "Removes an existing entry from a map.") { map =>
          map - 0
        }
      })

    if (benchmarkMap) add("Immutable Map 2",
      "Immutable Map methods that operate on all elements.",
      "N elements",
      0 to 10,
      0 to 30,
      new FunBenchmarks[Int, Map[Int, String]] {
        prepare {
          length => TestData.map(length)
        }
        run("filterKeys", "Filters a map by removing all odd keys.") { map =>
          map.filterKeys(_ % 2 == 0).size
        }
      })

    if (benchmarkMap) add("Immutable Map 3",
      "Immutable Map methods that operate on all elements.",
      "N elements",
      0 to 10,
      0 to 200,
      new FunBenchmarks[Int, Map[Int, String]] {
        prepare {
          length => TestData.map(length)
        }
        run("filterKeys", "Filters a map by removing all odd keys.") { map =>
          map.filterKeys(_ % 2 == 0).size
        }
      })

    if (benchmarkMap || benchmarkJava) add("HashMap read",
      "Basic HashMap methods.",
      "N elements",
      0 to 3,
      0 to 100,
      new FunBenchmarks[Int, HashMap[Int, String]] {
        override val runLoopCount = 1
        prepare {
          length => TestData.hashMap(length)
        }
        run("contains true", "Checks that a map really contains a value.") { map =>
          map.containsKey(0)
        }
        run("contains false", "Checks that a map really does not contain a value.") { map =>
          map.containsKey(-999)
        }
        run("size", "Calculates the size of a map.") { map =>
          map.size()
        }
      })

    if (benchmarkMap || benchmarkJava) add("Large HashMap read",
      "Basic HashMap methods.",
      "N elements",
      0 to 3,
      0 to 1000 by 20,
      new FunBenchmarks[Int, HashMap[Int, String]] {
        override val runLoopCount = 1
        prepare {
          length => TestData.hashMap(length)
        }
        run("contains true", "Checks that a map really contains a value.") { map =>
          map.containsKey(0)
        }
        run("contains false", "Checks that a map really does not contain a value.") { map =>
          map.containsKey(-999)
        }
        run("size", "Calculates the size of a map.") { map =>
          map.size()
        }
      })

    if (benchmarkMap || benchmarkJava) add("HashMap write",
      "Basic HashMap methods.",
      "N elements",
      0 to 3,
      0 to 1000 by 20,
      new FunBenchmarks[Int, HashMap[Int, String]] {
        override val runLoopCount = 1
        prepare {
          length => TestData.hashMap(length)
        }
        run("put", "Adds a new entry to a map.") { map =>
          map.put(-1, "X-1")
        }
        run("remove", "Removes an existing entry from a map.") { map =>
          map.remove(0)
        }
        run("clear", "Removes all entries from a map.") { map =>
          map.clear()
        }
      })

    if (benchmarkMap || benchmarkJava) add("ConcurrentHashMap read",
      "ConcurrentHashMap methods that read from the map.",
      "N elements",
      0 to 1000 by 20,
      new FunBenchmarks[Int, ConcurrentHashMap[Int, String]] {
        override val runLoopCount = 1
        prepare {
          length => TestData.concurrentHashMap(length)
        }
        run("contains true", "Checks that a map really contains a value.") { map =>
          map.containsKey(0)
        }
        run("contains false", "Checks that a map really does not contain a value.") { map =>
          map.containsKey(-999)
        }
        run("size", "Calculates the size of a map.") { map =>
          map.size()
        }
      })

    if (benchmarkMap || benchmarkJava) add("ConcurrentHashMap write",
      "ConcurrentHashMap methods that write into the map.",
      "N elements",
      0 to 1000 by 20,
      new FunBenchmarks[Int, ConcurrentHashMap[Int, String]] {
        override val runLoopCount = 1
        prepare {
          length => TestData.concurrentHashMap(length)
        }
        run("remove", "Removes an existing entry from a map.") { map =>
          map.remove(0)
        }
        run("put", "Adds a new entry to a map.") { map =>
          map.put(-1, "X-1")
        }
        run("clear", "Removes all entries from a map.") { map =>
          map.clear()
        }
      })

    if (benchmarkLong && benchmarkMap) add("Large Immutable Map",
      "Immutable Map methods with a large map.",
      "N elements",
      0 to 10,
      0 to 1000 by 10,
      new FunBenchmarks[Int, Map[Int, String]] {
        prepare {
          length => TestData.map(length)
        }
        run("contains true", "Checks that a map really contains a value.") { map =>
          map.contains(0)
        }
        run("contains false", "Checks that a map really does not contain a value.") { map =>
          map.contains(-999)
        }
        run("+", "Adds a new entry to a map.") { map =>
          map + (-1 -> "X-1")
        }
        run("-", "Removes an existing entry from a map.") { map =>
          map - 0
        }
      })

    if (benchmarkMap) add("Mutable Map",
      "Mutable Map methods that operate on a single element.",
      "N elements",
      0 to 30,
      new FunBenchmarks[Int, scala.collection.mutable.Map[Int, String]] {
        override val runLoopCount = 1
        prepare {
          length => TestData.mutableMap(length)
        }
        run("contains true", "Checks that a map really contains a value.") { map =>
          map.contains(0)
        }
        run("contains false", "Checks that a map really does not contain a value.") { map =>
          map.contains(-999)
        }
        run("+=", "Adds a new entry to a map.") { map =>
          map += (-1 -> "X-1")
        }
        run("-=", "Removes an existing entry from a map.") { map =>
          map -= 0
        }
      })

    if (benchmarkMap) add("Large Mutable Map",
      "Mutable Map methods that operate on a single element.",
      "N elements",
      0 to 1000 by 20,
      new FunBenchmarks[Int, scala.collection.mutable.Map[Int, String]] {
        override val runLoopCount = 1
        prepare {
          length => TestData.mutableMap(length)
        }
        run("contains true", "Checks that a map really contains a value.") { map =>
          map.contains(0)
        }
        run("contains false", "Checks that a map really does not contain a value.") { map =>
          map.contains(-999)
        }
        run("+=", "Adds a new entry to a map.") { map =>
          map += (-1 -> "X-1")
        }
        run("-=", "Removes an existing entry from a map.") { map =>
          map -= 0
        }
      })

    if (benchmarkMap) add("Mutable Map2",
      "Mutable Map methods that operate on all elements.",
      "N elements",
      0 to 30,
      new FunBenchmarks[Int, scala.collection.mutable.Map[Int, String]] {
        override val runLoopCount = 1
        prepare {
          length => TestData.mutableMap(length)
        }
        run("filterKeys", "Filters a map by removing all odd keys.") { map =>
          map.filterKeys(_ % 2 == 0).size
        }
      })

    if (benchmarkMap) add("Large Mutable Map2",
      "Mutable Map methods that operate on all elements.",
      "N elements",
      0 to 30,
      new FunBenchmarks[Int, scala.collection.mutable.Map[Int, String]] {
        override val runLoopCount = 1
        prepare {
          length => TestData.mutableMap(length)
        }
        run("filterKeys", "Filters a map by removing all odd keys.") { map =>
          map.filterKeys(_ % 2 == 0).size
        }
      })

    if (benchmarkPattern) add("Pattern Matching",
      "Different examples for pattern matching and equivalent code.",
      "N elements",
      0 to 20,
      0 until 100 by 10,
      new FunBenchmarks[Int, Seq[Int]] {
        prepare {
          length => TestData.list(length, 10)
        }
        run("match 1", "Matches the 1st pattern with a literal integer value.") { seq =>
          for (value <- seq) {
            value match {
              case 1 => "one"
              case _ => "anything"
            }
          }
        }
        run("if 1", "Matches the 1st if in an if-else cascade with integer values.") { seq =>
          for (value <- seq) {
            if (value == 1) "one"
            else "anything"
          }
        }
        run("match 5", "Matches the 5th pattern with a literal integer value.") { seq =>
          for (value <- seq) {
            value match {
              case 1 => "one"
              case 2 => "two"
              case 3 => "three"
              case 4 => "four"
              case 5 => "five"
              case _ => "anything"
            }
          }
        }
        run("if 5", "Matches the 5th if in an if-else cascade with integer values.") { seq =>
          for (value <- seq) {
            if (value == 1) "one"
            else if (value == 2) "two"
            else if (value == 3) "three"
            else if (value == 4) "four"
            else if (value == 5) "five"
            else "anything"
          }
        }
      })

    report()
  }

  object ExampleSingleton {
    def singletonMethod(x: Int) = x + x
  }

  trait ExampleTrait {
    def traitMethod(x: Int): Int
    def traitMethodImpl(x: Int) = x + x
  }

  object ExampleTraitSingleton extends ExampleTrait {
    def traitMethod(x: Int) = x + x
  }
}
