package ch.obermuhlner.scala.microbenchmark

import scala.collection.mutable.ListBuffer

trait ImageReport {
  protected var runner: BenchmarkRunner = new BenchmarkRunner(new ConsoleLog(true))

  protected var chartWidth = 800
  protected var chartHeight = 600

  def lineChart[T, V](title: String, warmupDatasets: Seq[T], datasets: Seq[T], suite: FunBenchmarks[T, V]): String = {
    lineChart(title, "Data", warmupDatasets, datasets, suite)
  }

  def lineChart[T, V](title: String, datasetAxisLabel: String, warmupDatasets: Seq[T], datasets: Seq[T], suite: Suite[T, V]): String = {
    val suiteResults = runner.execute(warmupDatasets, datasets, suite.benchmarks)
    lineChart(title, datasetAxisLabel, suiteResults)
  }

  def lineChart[T](title: String, datasetAxisLabel: String, results: Seq[Seq[Result[T]]]): String = {
    lineChart(title, datasetAxisLabel, 0.95, results)
  }

  def lineChart[T](title: String, datasetAxisLabel: String, bestPart: Double, results: Seq[Seq[Result[T]]]): String = {
    val chart = new LineChart(title) with BestResultTime[T]
    chart.categoryAxisLabel = datasetAxisLabel
    chart.bestPart = bestPart
    chart.addSuiteResults(results)

    val filename = title + "_" + bestPart + ".png"
    ChartWriter.writeToPng(chart, filename, chartWidth, chartHeight)
    filename
  }

  def statisticalLineChart[T](title: String, datasetAxisLabel: String, results: Seq[Seq[Result[T]]]): String = {
    val chart = new LineChart(title, true, true) with StatisticalResultTime[T]
    chart.categoryAxisLabel = datasetAxisLabel
    chart.addSuiteResults(results)

    val filename = title + ".png"
    ChartWriter.writeToPng(chart, filename, chartWidth, chartHeight)
    filename
  }

  def barChart[T](name: String, title: String, result: Result[T]): String = {
    val chart = new BarChart(title) with BestResultTime[T]
    chart.categoryAxisLabel = "Elapsed Time"
    chart.addResult(result)

    val filename = title + ".png"
    ChartWriter.writeToPng(chart, filename, chartWidth, chartHeight)
    filename
  }

  def allMeasurementsChart[T, V](title: String, warmupDatasets: Seq[T], datasets: Seq[T], suite: Suite[T, V]): String = {
    val suiteResults = runner.execute(warmupDatasets, datasets, suite.benchmarks)
    allSuiteMeasurementsChart(title, false, suiteResults)
  }

  def allSuiteMeasurementsChart[T, V](title: String, sorted: Boolean, suiteResults: Seq[Seq[Result[T]]]): String = {
    val chart = new LineChart(title) with AllResultTimes[T]

    for (results <- suiteResults) {
      for (result <- results) {
        val resultName = result.name + " " + result.dataset
        chart.addResult(result, resultName)
        if (sorted) {
          chart.addResult(result, "Sorted " + resultName, seq => seq.sorted)
        }
      }
    }

    val filename = title + ".png"
    ChartWriter.writeToPng(chart, filename, chartWidth, chartHeight)
    filename
  }

  def allMeasurementsChart[T, V](title: String, sorted: Boolean, results: Seq[Result[T]]): String = {
    allMeasurementsChart(title, title, sorted, results)
  }

  def allMeasurementsChart[T, V](name: String, title: String, sorted: Boolean, results: Seq[Result[T]]): String = {
    val legend = results.length <= 30
    val chart = new LineChart(title, false, legend) with AllResultTimes[T]

    for (result <- results) {
      val resultName = result.name + " " + result.dataset
      chart.addResult(result, resultName)
      if (sorted) {
        chart.addResult(result, resultName, seq => seq.sorted)
      }
    }

    val filename = name + ".png"
    ChartWriter.writeToPng(chart, filename, chartWidth, chartHeight)
    filename
  }

  def allMeasurementsCharts[T, V](title: String, warmupDatasets: Seq[T], datasets: Seq[T], suite: Suite[T, V]): Seq[String] = {
    val files = new ListBuffer[String]

    val suiteResults = runner.execute(warmupDatasets, datasets, suite.benchmarks)
    for (results <- suiteResults) {
      var i = 0
      for (result <- results) {
        val chartName = title + "_" + result.name + "_" + i.formatted("")
        val chartTitle = title + " " + result.name + " " + result.dataset
        files += allMeasurementsChart(chartName, chartTitle, false, result)
        i += 1
      }
    }

    files.toList
  }

  def allMeasurementsChart[T](name: String, title: String, sorted: Boolean, result: Result[T]): String = {
    val legend = result.elapsedTimes.length <= 30
    val chart = new LineChart(title, false, legend) with AllResultTimes[T]
    chart.categoryAxisLabel = "Elapsed Time"
    chart.addResult(result)
    if (sorted) {
      chart.addResult(result, result.name + " (sorted)", times => times.sorted)
    }

    val filename = name + ".png"
    ChartWriter.writeToPng(chart, filename, chartWidth, chartHeight)
    filename
  }

  def histogramChart[T](name: String, title: String, bins: Int, result: Result[T]): String = {
    val chart = new HistogramChart(title) with HistogramResultTimes[T]
    chart.categoryAxisLabel = "Elapsed Time"
    chart.addResult(result, bins)

    val filename = name + ".png"
    ChartWriter.writeToPng(chart, filename, chartWidth, chartHeight)
    filename
  }
}

