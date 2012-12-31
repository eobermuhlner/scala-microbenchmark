package ch.obermuhlner.scala.microbenchmark.example
import ch.obermuhlner.scala.microbenchmark.Statistics

object StatisticsExample {

  def main(args: Array[String]) {
    val data = List(2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0)
    val statistics = new Statistics(data)
    println("Standard Deviation: " + statistics.standardDeviation)
  }
}