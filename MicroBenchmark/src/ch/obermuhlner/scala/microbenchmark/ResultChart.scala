package ch.obermuhlner.scala.microbenchmark
import com.obermuhlner.scala.microbenchmark._

/*
 * linechart showing Seq[elapsedTime] in 1 Result
 * - category = benchmark.name
 * - x axis = dataset
 * - y axis = ms | TPS
 * 
 * linechart showing elapsedTime of Seq[Result] over Seq[dataset]
 * - category = benchmark.name
 * - x axis = dataset
 * - y axis = ms | TPS
 * 
 * barchart showing Seq[elapsedTime] of 1 Result over 1 dataset
 * - x axis = benchmark.name
 * - y axis = ms | TPS
 * 
 * barchart showing 1 elapsedTime of Seq[Result] over 1 dataset
 * - x axis = benchmark.name
 * - y axis = ms | TPS
 * 
 * barchart showing 1 elapsedTime of 1 Result over Seq[dataset]
 * - x axis = dataset
 * - y axis = ms | TPS
 * 
 * barchart showing 1 elapsedTime of Seq[Result] over Seq[dataset] (unusual)
 * - category = benchmark.name
 * - x axis = benchmark.name + dataset
 * - y axis = ms | TPS
 */

trait BestResultTime[T] extends CategoryDatasetChart {
  valueAxisLabel = "ms"

  var bestPart = 0.95

  var defaultFilter: Seq[Double] => Double = elapsedTimes => new Statistics(new Statistics(elapsedTimes).minPart(bestPart)).average

  def addSuiteResults(suiteResults: Seq[Seq[Result[T]]]) {
    addSuiteResults(suiteResults, defaultFilter)
  }

  def addSuiteResults(suiteResults: Seq[Seq[Result[T]]], filter: Seq[Double] => Double) {
    for (results <- suiteResults) {
      addResults(results, filter)
    }
  }

  def addResults(results: Seq[Result[T]]) {
    addResults(results, defaultFilter)
  }

  def addResults(results: Seq[Result[T]], filter: Seq[Double] => Double) {
    for (result <- results) {
      addResult(result, result.name, filter)
    }
  }

  def addResult(result: Result[T]) {
    addResult(result, result.name, defaultFilter)
  }

  def addResult(result: Result[T], name: String, filter: Seq[Double] => Double) {
    val elapsedTime = filter(result.elapsedTimes)
    val rowName = name
    val colName = result.dataset.toString

    addValue(elapsedTime, rowName, colName)
  }
}

trait StatisticalResultTime[T] extends CategoryDatasetChart {
  valueAxisLabel = "ms"

  def addSuiteResults(suiteResults: Seq[Seq[Result[T]]]) {
    for (results <- suiteResults) {
      addResults(results)
    }
  }

  def addResults(results: Seq[Result[T]]) {
    for (result <- results) {
      addResult(result)
    }
  }

  def addResult(result: Result[T]) {
    val statistics = new Statistics(result.elapsedTimes)
    val rowName = result.name
    val colName = result.dataset.toString

    addValue(statistics.average, statistics.standardDeviation, rowName, colName)
  }

}

trait AllResultTimes[T] extends CategoryDatasetChart {
  valueAxisLabel = "ms"

  var filter: Seq[Double] => Seq[Double] = times => times

  def addResult(result: Result[T]) {
    addResult(result, result.name, filter)
  }

  def addResult(result: Result[T], name: String) {
    addResult(result, name, filter)
  }

  def addResult(result: Result[T], name: String, filter: Seq[Double] => Seq[Double]) {
    var i = 0
    for (elapsedTime <- filter(result.elapsedTimes)) {
      val rowName = name
      val colName = i.toString

      addValue(elapsedTime, rowName, colName)
      i += 1
    }
  }
}

trait HistogramResultTimes[T] extends HistogramDatasetChart {
  def addResult(result: Result[T], bins: Int) {
    val array = new Array[Double](result.elapsedTimes.size)
    for (i <- 0 until result.elapsedTimes.size) {
      array(i) = result.elapsedTimes(i)
    }
    addSeries(result.name, array, bins)
  }
}
