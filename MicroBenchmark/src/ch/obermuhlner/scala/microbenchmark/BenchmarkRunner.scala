package ch.obermuhlner.scala.microbenchmark

import scala.collection.mutable.ListBuffer

object StatisticsUtil {
  def average(list: Seq[Double]): Double = {
    list.sum / list.length
  }

  def median(list: Seq[Double]): Double = {
    val count = list.length
    if (count == 0) {
      return 0;
    }
    if (count == 1) {
      return list(0);
    }

    val sortedList = list.sorted
    if (count % 2 == 0) {
      return (sortedList(count / 2) + sortedList(count / 2 + 1)) / 2.0
    } else {
      return sortedList(count / 2)
    }
  }

  def medianPart(list: Seq[Double], part: Double): Seq[Double] = {
    val count = list.length
    val startCount = (count * part).toInt
    val endCount = (count - startCount).toInt

    list.sorted.slice(startCount, endCount)
  }

  def minPart(list: Seq[Double], part: Double): Seq[Double] = {
    val count = list.length
    val endCount = (count * part).toInt

    list.sorted.slice(0, endCount)
  }
}

abstract class Log {
  def warmup[T, V](benchmark: Benchmark[T, V], dataset: T, warmupTimes: Seq[Double])
  def elapsed[T, V](benchmark: Benchmark[T, V], dataset: T, elapsedTimes: Seq[Double])
}

class ConsoleLog(var verbose: Boolean) extends Log {
  def warmup[T, V](benchmark: Benchmark[T, V], dataset: T, warmupTimes: Seq[Double]) {
    if (verbose) {
      val totalTime = warmupTimes.sum
      val name = benchmark.name + "(" + dataset + ")"
      println(name + " warmup " + warmupTimes.length + " runs in " + totalTime + " ms")
    }
  }
  def elapsed[T, V](benchmark: Benchmark[T, V], dataset: T, elapsedTimes: Seq[Double]) {
    val statistics = new Statistics(elapsedTimes)
    val name = benchmark.name + "(" + dataset + ")"
    val best = new Statistics(statistics.minPart(0.5)).average
    println(name + " : " + best + " ms (" + statistics.length + " runs, avg=" + statistics.average + ", median=" + statistics.median + ", min=" + statistics.min + ", max=" + statistics.max + ")")
  }
}

class BenchmarkRunner(val log: Log) {
  var singleRunTimeThreshold = 10000.0

  var warmupTimeThreshold = 1000.0
  var warmupCount = 1100

  var runTimeThreshold = 1000.0
  var runCount = 100

  var runLoopCount = 10

  var useReferenceRun = false

  var forceGarbageCollection = false

  var filter: Seq[Double] => Seq[Double] = times => new Statistics(times).minPartUnsorted(0.95)

  def execute[T, V](warmupDatasets: Seq[T], datasets: Seq[T], benchmarks: Seq[Benchmark[T, V]]): Seq[Seq[Result[T]]] = {
    val results = new ListBuffer[Seq[Result[T]]]()
    for (benchmark <- benchmarks) {
      results += execute(warmupDatasets, datasets, benchmark)
    }
    results.toList
  }

  def execute[T, V](warmupDatasets: Seq[T], datasets: Seq[T], benchmark: Benchmark[T, V]): Seq[Result[T]] = {
    val results = new ListBuffer[Result[T]]()
    for (warmupDataset <- warmupDatasets) {
      warmup(warmupDataset, benchmark)
    }
    for (dataset <- datasets) {
      results += execute(warmupDatasets.length == 0, dataset, benchmark)
    }
    results.toList
  }

  def warmup[T, V](dataset: T, benchmark: Benchmark[T, V]) = {
    val (warmupTimes, result) = execute(benchmark, dataset, warmupCount, warmupTimeThreshold)
    log.warmup(benchmark, dataset, warmupTimes)
  }

  def execute[T, V](warmup: Boolean, dataset: T, benchmark: Benchmark[T, V]): Result[T] = {
    if (warmup) {
      val (firstTime, firstResult) = elapsedTime(benchmark, 0, dataset);
      log.warmup(benchmark, dataset, List(firstTime))
      if (firstTime >= singleRunTimeThreshold) {
        log.elapsed(benchmark, dataset, List(firstTime))
        return new Result(benchmark.name, benchmark.description, dataset, List(firstTime), firstResult)
      }

      val (warmupTimes, result) = execute(benchmark, dataset, warmupCount, warmupTimeThreshold)
      log.warmup(benchmark, dataset, warmupTimes)
    }

    val (runTimes, result) = execute(benchmark, dataset, runCount, runTimeThreshold)
    log.elapsed(benchmark, dataset, runTimes)

    new Result(benchmark.name, benchmark.description, dataset, filter(runTimes), result)
  }

  def execute[T, V](benchmark: Benchmark[T, V], dataset: T, maxRunCount: Int, timeThreshold: Double): (Seq[(Double)], Any) = {
    val elapsedTimes = new ListBuffer[Double]
    var totalTime = 0.0
    var count = 0
    var result: Any = "n/a"

    gc()
    while (count < maxRunCount && totalTime < timeThreshold) {
      val referenceNanos = if (useReferenceRun) elapsedNanosReferenceRun(benchmark, dataset) else 0
      val (time, singleResult) = elapsedTime(benchmark, referenceNanos, dataset)

      count += 1
      totalTime += time
      elapsedTimes += time

      result = singleResult
    }

    (elapsedTimes.toList, result)
  }

  private def elapsedNanosReferenceRun[T, V](benchmark: Benchmark[T, V], dataset: T) = {
    val preparedDataset = benchmark.prepare(dataset)
    val loopCount = mergedRunLoopCount(benchmark)
    var result: Any = ""
    val startTime = System.nanoTime

    var i = 0;
    while (i < loopCount) {
      result = benchmark.referenceRun(preparedDataset)
      i += 1;
    }

    val endTime = System.nanoTime
    val deltaTime = endTime - startTime

    deltaTime
  }

  private def elapsedTime[T, V](benchmark: Benchmark[T, V], referenceNanos: Long, dataset: T) = {
    val preparedDataset = benchmark.prepare(dataset)
    val loopCount = mergedRunLoopCount(benchmark)
    var result: Any = ""
    val startTime = System.nanoTime

    var i = 0;
    while (i < loopCount) {
      result = benchmark.run(preparedDataset)
      i += 1;
    }

    val endTime = System.nanoTime
    val deltaTime = (endTime - startTime - referenceNanos) / 1000000.0 / benchmark.internalLoopCount / loopCount

    (deltaTime, result)
  }

  private def mergedRunLoopCount[T, V](benchmark: Benchmark[T, V]) = {
    if (benchmark.runLoopCount >= 0) benchmark.runLoopCount else runLoopCount
  }

  private def gc() {
    if (forceGarbageCollection) {
      System.gc()
    }
  }
}

