package ch.obermuhlner.scala.microbenchmark

abstract class Benchmark[T, V](val name: String, val description: String) {
  def this() {
    this(getClass().getSimpleName, "")
  }

  def this(name: String) {
    this(name, "")
  }

  val runLoopCount = -1
  val internalLoopCount = 1

  def prepare(dataset: T): V
  def referenceRun(dataset: V): Any = { dataset }
  def run(dataset: V): Any
}

case class Result[T](val name: String, val description: String, val dataset: T, val elapsedTimes: Seq[Double], val result: Any)

trait Suite[T, V] {
  def benchmarks: Seq[Benchmark[T, V]]
}

object Suite {
  def apply[T, V](suiteBenchmarks: Seq[Benchmark[T, V]]): Suite[T, V] = {
    new Suite[T, V]() {
      val benchmarks = suiteBenchmarks
    }
  }
}