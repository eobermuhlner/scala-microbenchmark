package ch.obermuhlner.scala.microbenchmark.example
import ch.obermuhlner.scala.microbenchmark.LineChart
import ch.obermuhlner.scala.microbenchmark.ChartWriter

object ChartExample {
  val width = 800
  val height = 600

  def main(args: Array[String]) {
    exampleLineChart
  }

  def exampleLineChart {
    val chart = new LineChart("LineChart")
    chart.categoryAxisLabel = "columns"

    chart.addValue(7, "row1", "col1")
    chart.addValue(5, "row1", "col2")
    chart.addValue(9, "row1", "col3")
    chart.addValue(8, "row1", "col4")

    chart.addValue(12, "row2", "col1")
    chart.addValue(3, "row2", "col2")

    val filename = "LineChart" + ".png"
    ChartWriter.writeToPng(chart, filename, width, height)
  }
}