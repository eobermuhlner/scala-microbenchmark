package ch.obermuhlner.scala.microbenchmark.example

import ch.obermuhlner.scala.microbenchmark._

object SimpleExample {
  def main(args: Array[String]) {
    val runner = new BenchmarkRunner(new ConsoleLog(false))
    runner.warmupCount = 1000
    runner.runCount = 1000

    val suite = Suite(
      List(
        new Benchmark[Int, Int]("for") {
          def prepare(count: Int) = count
          def run(count: Int) = {
            for (i <- 0 to count) {
            }
          }
        },
        new Benchmark[Int, Int]("while") {
          def prepare(count: Int) = count
          def run(count: Int) = {
            var i = 0
            while (i < count) {
              i += 1
            }
          }
        }))

    val suiteResults = runner.execute(List(10), 0 to 10000 by 1000, suite.benchmarks)

    val loopsChart = new LineChart("Loops") with BestResultTime[Int]
    loopsChart.categoryAxisLabel = "Loop Count"
    //loopsChart.addSuiteResults(suiteResults)
    loopsChart.addSuiteResults(suiteResults, times => StatisticsUtil.average(StatisticsUtil.minPart(times, 0.5)))
    ChartWriter.writeToPng(loopsChart, "Loops.png", 800, 600)

    val detailChart = new LineChart("Loop (all measurements)") with AllResultTimes[Int]
    detailChart.categoryAxisLabel = "Elapsed time"
    detailChart.addResult(suiteResults(0)(9))
    detailChart.addResult(suiteResults(0)(9), "sorted", times => times.sorted)
    ChartWriter.writeToPng(detailChart, "LoopsAllMeasurements.png", 800, 600)

    val comparisonChart = new BarChart("Loops Comparison") with BestResultTime[Int]
    detailChart.categoryAxisLabel = "Loop Implementation"
    comparisonChart.addResult(suiteResults(0)(9))
    comparisonChart.addResult(suiteResults(1)(9))
    ChartWriter.writeToPng(comparisonChart, "LoopsComparison.png", 800, 600)
  }
}
