package ch.obermuhlner.scala.microbenchmark

/**
 * Provides statistical information about a sequence of double values.
 */
class Statistics(data: Seq[Double]) {
  private val sorted = data.sorted

  val length = data.length
  val min = if (length == 0) 0 else data.min
  val max = if (length == 0) 0 else data.max
  val sum = if (length == 0) 0 else data.sum
  val average = if (length == 0) 0 else sum / length
  val variance = if (length == 0) 0 else {
    var result = 0.0
    for (v <- data) {
      result += (average - v) * (average - v)
    }
    result / length
  }
  val standardDeviation = Math.sqrt(variance)

  val median = {
    if (length == 0) {
      0
    } else {
      if (length % 2 == 0) {
        (sorted(length / 2 - 1) + sorted(length / 2)) / 2
      } else {
        sorted(length / 2)
      }
    }
  }

  def medianPart(part: Double): Seq[Double] = {
    require(part >= 0.0)
    require(part <= 1.0)

    val startCount = (length * part).toInt
    val endCount = (length - startCount).toInt

    sorted.slice(startCount, endCount)
  }

  def minPart(part: Double): Seq[Double] = {
    require(part >= 0.0)
    require(part <= 1.0)

    val endCount = Math.min(length, (length * part + 1).toInt)

    sorted.slice(0, endCount)
  }

  def minPartUnsorted(part: Double): Seq[Double] = {
    val partialSeq: Seq[Double] = minPart(part)
    val threshold = if (partialSeq.length == 0) 0 else partialSeq.max
    for (v <- data if v <= threshold) yield v
  }

}