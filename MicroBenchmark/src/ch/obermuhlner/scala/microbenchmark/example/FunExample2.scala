package ch.obermuhlner.scala.microbenchmark.example
import ch.obermuhlner.scala.microbenchmark.FunBenchmarks
import ch.obermuhlner.scala.microbenchmark.ImageReport

object FunExample2 extends ImageReport {
  def main(args: Array[String]) {
    //runner.runCount = 1000

    lineChart("CompilerOptimizations", 0 to 1, 0 to 10000 by 1000,
      new FunBenchmarks[Int, Int] {
        var result: Int = 0
        prepare {
          count => count
        }
        run("empty loop") { count =>
          for (i <- 0 to count) {
          }
        }
        run("empty loop result") { count =>
          for (i <- 0 to count) {
            result += i
          }
        }
      })

    lineChart("Loops", 0 to 1, 0 to 100000 by 10000,
      new FunBenchmarks[Int, Int] {
        prepare {
          count => count
        }
        run("for") { count =>
          for (i <- 0 to count) {
          }
        }
        run("while") { count =>
          var i = 0
          while (i < count) {
            i += 1
          }
        }
      })

    lineChart("Casts", 0 to 1, 0 to 100000 by 10000,
      new FunBenchmarks[Int, Int] {
        var any: Any = "Hello"
        var result: String = _

        prepare {
          count => count
        }
        run("asInstanceOf") { count =>
          for (i <- 0 to count) {
            result = any.asInstanceOf[String]
          }
        }
        run("match case") { count =>
          for (i <- 0 to count) {
            result = any match {
              case s: String => s
              case _ => throw new IllegalArgumentException
            }
          }
        }
      })

    lineChart("Calls", 0 to 1, 0 to 100000 by 10000,
      new FunBenchmarks[Int, Int] {
        prepare {
          count => count
        }
        run("empty loop") { count =>
          for (i <- 0 to count) {
          }
        }
        run("method") { count =>
          for (i <- 0 to count) {
            anotherMethod()
          }
        }
        run("static") { count =>
          for (i <- 0 to count) {
            ExampleStatic.staticMethod()
          }
        }
        run("trait") { count =>
          for (i <- 0 to count) {
            ExampleTraitObject.traitMethod()
          }
        }
        run("traitImpl") { count =>
          for (i <- 0 to count) {
            ExampleTraitObject.traitMethodImpl()
          }
        }

        def anotherMethod() {
        }
      })

    lineChart("List", "N elements", 0 to 1, 0 until 1000 by 10,
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
        run("mkString") { list =>
          list.mkString
        }
      })

    println("Finished")
  }

  object ExampleStatic {
    def staticMethod() {}
  }

  trait ExampleTrait {
    def traitMethod()
    def traitMethodImpl() {}
  }

  object ExampleTraitObject extends ExampleTrait {
    def traitMethod() {}
  }

}
