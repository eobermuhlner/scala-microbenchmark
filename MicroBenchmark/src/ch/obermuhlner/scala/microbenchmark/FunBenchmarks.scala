package ch.obermuhlner.scala.microbenchmark

import scala.collection.mutable.ListBuffer

trait FunBenchmarks[T, V] extends Suite[T, V] {

  val benchmarksBuffer = new ListBuffer[Benchmark[T, V]]

  var prepareFunction: (T) => V = _

  var detailChartPrefix: String = _
  var detailChartWidth: Int = _
  var detailChartHeight: Int = _

  val runLoopCount = 1
  val internalLoopCount = 1

  def detailChart(prefix: String, width: Int, height: Int) {
    detailChartPrefix = prefix
    detailChartWidth = width
    detailChartHeight = height
  }

  def prepare(block: (T) => V) {
    prepareFunction = block
  }

  def run(name: String)(block: (V) => Any) {
    run(name, "")(block);
  }

  def run(name: String, description: String)(block: (V) => Any) {
    val benchmark = new Benchmark[T, V](name, description) {
      override val runLoopCount = FunBenchmarks.this.runLoopCount;
      override val internalLoopCount = FunBenchmarks.this.internalLoopCount;

      override def prepare(dataset: T): V = {
        prepareFunction(dataset)
      }
      override def run(dataset: V) = {
        block(dataset)
      }
    }

    benchmarksBuffer += benchmark
  }

  def benchmarks: Seq[Benchmark[T, V]] = {
    benchmarksBuffer.toList
  }
}

