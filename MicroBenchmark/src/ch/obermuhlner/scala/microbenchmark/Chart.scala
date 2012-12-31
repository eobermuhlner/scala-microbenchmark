package ch.obermuhlner.scala.microbenchmark

import org.jfree.chart._
import org.jfree.data.category._
import org.jfree.chart.plot._
import org.jfree.chart.axis._
import org.jfree.chart.renderer.category._
import org.jfree.data.statistics._
import java.io.File
import com.obermuhlner.scala.microbenchmark._

abstract class Chart(val title: String) {
  def createChart(): JFreeChart
}

abstract class HistogramDatasetChart(title: String) extends Chart(title) {
  protected val dataset = new HistogramDataset

  var categoryAxisLabel: String = ""
  var valueAxisLabel: String = ""

  def addSeries(columnKey: Comparable[_], values: Array[Double], bins: Int) {
    dataset.addSeries(columnKey, values, bins)
  }
}

abstract class CategoryDatasetChart(title: String) extends Chart(title) {
  protected val dataset = new DefaultStatisticalCategoryDataset

  var categoryAxisLabel: String = ""
  var valueAxisLabel: String = ""

  def addValue(value: Double, columnKey: Comparable[_], rowKey: Comparable[_]) {
    dataset.add(value, 0, columnKey, rowKey)
  }

  def addValue(value: Double, stdDeviation: Double, columnKey: Comparable[_], rowKey: Comparable[_]) {
    dataset.add(value, stdDeviation, columnKey, rowKey)
  }
}

class LineChart(title: String, statistical: Boolean, legend: Boolean) extends CategoryDatasetChart(title) {
  def this(title: String) = this(title, false, true)

  def createChart(): JFreeChart = {
    val categoryAxis = new CategoryAxis(categoryAxisLabel)
    val valueAxis = new NumberAxis(valueAxisLabel)

    val renderer = if (statistical) new StatisticalLineAndShapeRenderer(true, true) else new LineAndShapeRenderer(true, true)
    val plot = new CategoryPlot(dataset, categoryAxis, valueAxis, renderer)
    plot.setOrientation(PlotOrientation.VERTICAL)

    new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, legend)
  }
}

class HistogramChart(title: String, statistical: Boolean) extends HistogramDatasetChart(title) {
  def this(title: String) = this(title, false)

  def createChart(): JFreeChart = {
    val categoryAxis = new CategoryAxis(categoryAxisLabel)
    val valueAxis = new NumberAxis(valueAxisLabel)

    ChartFactory.createHistogram(title, categoryAxisLabel, valueAxisLabel, dataset, PlotOrientation.VERTICAL, true, false, false)
  }
}

class BarChart(title: String) extends CategoryDatasetChart(title) {
  def createChart(): JFreeChart = {
    ChartFactory.createBarChart(title, categoryAxisLabel, valueAxisLabel, dataset, PlotOrientation.VERTICAL, true, false, false)
  }
}

object ChartWriter {
  def writeToPng(chart: Chart, filename: String, width: Int, height: Int) {
    ChartUtilities.saveChartAsPNG(new File(filename), chart.createChart, width, height)
  }
}
