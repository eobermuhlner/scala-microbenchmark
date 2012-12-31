package ch.obermuhlner.scala.microbenchmark.example
import ch.obermuhlner.scala.microbenchmark.Benchmark
import ch.obermuhlner.scala.microbenchmark.Suite
import ch.obermuhlner.scala.microbenchmark.FunBenchmarks
import ch.obermuhlner.scala.microbenchmark.HtmlReport

import scala.collection.mutable.ListBuffer

object HtmlExample1 extends HtmlReport {
  def main(args: Array[String]) {

    runner.warmupCount = 1000
    runner.runCount = 1000

    add("List", "List methods that operate on all elements", "N elements", 0 until 1000 by 100,
      new FunBenchmarks[Int, List[Int]] {
        prepare {
          length => TestData.list(length)
        }
        run("sort") { list =>
          list.sorted
        }
        run("reverse") { list =>
          list.reverse
        }
        run("length") { list =>
          list.length
        }
      })

    report()
  }
}
