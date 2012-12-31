package ch.obermuhlner.scala.microbenchmark.example
import ch.obermuhlner.scala.microbenchmark.FunBenchmarks
import ch.obermuhlner.scala.microbenchmark.ImageReport

object FunExample1 extends ImageReport {
  def main(args: Array[String]) {
    runner.warmupCount = 0
    runner.runCount = 1000

    val suiteFor = new FunBenchmarks[Int, Int] {
      prepare {
        count => count
      }
      run("for") { count =>
        for (i <- 0 to count) {
        }
      }
    }

    //allMeasurementsChart("Measurements For 1", 0 to 100000 by 25000, suiteFor)
    allMeasurementsChart("Measurements For 2", List(10), List(10000), suiteFor)
    //lineChart("For", 0 to 100000 by 10000, suiteFor)

    println("Finished")
  }
}
