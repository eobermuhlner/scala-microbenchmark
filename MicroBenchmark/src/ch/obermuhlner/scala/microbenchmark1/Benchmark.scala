package ch.obermuhlner.scala.microbenchmark1

import scala.collection.mutable.ListBuffer

import java.io.File

import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartUtilities
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.category.DefaultCategoryDataset

import org.jfree.chart.renderer.category.LineAndShapeRenderer
import org.jfree.chart.axis.CategoryAxis
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYItemRenderer
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.data.category.CategoryDataset

abstract class Benchmark[T] {
	val name = getClass.getSimpleName

	val description = ""
	
	val elapsedTimeFactor = 1
	
	def setUp(dataset: T) {}
	
	def tearDown(dataset: T) {}
	
	def referenceRun(dataset: T) {}
	
	def run(dataset: T)
}

abstract class SimpleBenchmark(override val name: String) extends Benchmark[Unit] {
	def this() {
		this(getClass.getSimpleName)
	}

	final override def setUp(dataset: Unit) {
		setUp();
	}
	
	final override def tearDown(dataset: Unit) {
		tearDown();
	}
	
	final override def referenceRun(dataset: Unit) {
		referenceRun();
	}
	
	final override def run(dataset: Unit) {
		run();
	}
	
	def setUp() {}
	
	def tearDown() {}
	
	def referenceRun() {}
	
	def run()
}

abstract class FunctionalBenchmark[T,V](override val name: String) extends Benchmark[T] {
	var dataset2: V = _

	def this() {
		this(getClass.getSimpleName)
	}

	final override def setUp(dataset: T) {
		dataset2 = prepare(dataset)
	}

	final override def run(dataset: T) {
		run2(dataset2)
	}

	final override def tearDown(dataset: T) {
		//dataset2 = null
	}

	def prepare(dataset: T): V

	def run2(dataset: V)
}

abstract class Log {
	def elapsed[T] (benchmark: Benchmark[T], dataset: T, elapsedTimes: Seq[Double])
	def warmup[T] (benchmark: Benchmark[T], dataset: T, warmupTimes: Seq[Double])
}

object Statistics {
	def average(list: Seq[Double]) : Double = {
		list.sum / list.length
	}

	def median(list: Seq[Double]) : Double = {
		val count = list.length
		if (count == 0) {
			return 0;
		}
		if (count == 1) {
			return list(0);
		}
		
		val sortedList = list.sorted
		if (count % 2 == 0) {
			return sortedList(count / 2)
		} else {
			return (sortedList(count / 2) + sortedList(count / 2 + 1)) / 2.0
		}
	}

	def medianPart(list: Seq[Double], part: Double): Seq[Double] = {
		val count = list.length
		val startCount = (count * part).toInt
		val endCount = (count - startCount).toInt

		val sortedList = list.sorted

		val result = new ListBuffer[Double]
		for(i <- startCount to endCount) {
			result += sortedList(i)
		}

		result.toList
	}
}

class ConsoleLog(var verbose: Boolean) extends Log {
	def elapsed[T] (benchmark: Benchmark[T], dataset: T, elapsedTimes: Seq[Double]) {
		val count = elapsedTimes.length
		val avgMedian50Time = Statistics.average(Statistics.medianPart(elapsedTimes, 0.5))
		val avgTime = Statistics.average(elapsedTimes)
		val medianTime = Statistics.median(elapsedTimes)
		val name = benchmark.name + "(" + dataset + ")"
		println(name + " : " + avgTime + " ms (" + count + " runs, avg50=" + avgMedian50Time + ", median=" + medianTime + ", min=" + elapsedTimes.min + " ,max=" + elapsedTimes.max + ")")
	}
		
	def warmup[T] (benchmark: Benchmark[T], dataset: T, warmupTimes: Seq[Double]) {
		if (verbose) {
			val totalTime = warmupTimes.sum
			val name = benchmark.name + "(" + dataset + ")"
			println(name + " warmup " + warmupTimes.length + " runs in " + totalTime + " ms")
		}
	}
}

abstract class ChartWriter(val name: String, val title: String) {
	def add[T] (results: Seq[Result[T]])

	def createChart(): String
}

class ConsoleChartWriter(name: String, title: String) extends ChartWriter(name, title) {
	def add[T] (results: Seq[Result[T]]) {
		println("### Chart " + title + " ###")
		println("Name,Dataset,AverageMedian50,Average,Median,Min,Max,Count")
		for(result <- results) {
			val count = result.elapsedTimes.length
			val avgMedian50Time = Statistics.average(Statistics.medianPart(result.elapsedTimes, 0.5))
			val avgTime = Statistics.average(result.elapsedTimes)
			val medianTime = Statistics.median(result.elapsedTimes)
			val minTime = result.elapsedTimes.min
			val maxTime = result.elapsedTimes.max
			println(result.name + "," + result.dataset + "," + avgMedian50Time + "," + avgTime + "," + medianTime + "," + minTime + "," + maxTime + "," + count)
		}
		println()
	}

	def createChart(): String = {
		name
	}
}

abstract class JFreeChartWriter(name: String, title: String) extends ChartWriter(name, title) {
	var width = 800
	var height = 600
	var categoryAxisLabel = "Benchmark"
	var valueAxisLabel = "ms"
}

class BarChartWriter(name: String, title: String) extends JFreeChartWriter(name, title) {
	val dataset = new DefaultCategoryDataset

	def add[T] (results: Seq[Result[T]]) {
		for(result <- results) {
			val elapsedTime = Statistics.average(Statistics.medianPart(result.elapsedTimes, 0.5))
			val rowName = result.name
			val colName = result.dataset.toString
			dataset.addValue(elapsedTime, rowName, colName)
		}

	}

	def createChart(): String = {
		val barChart = ChartFactory.createBarChart(title, categoryAxisLabel, valueAxisLabel, dataset, PlotOrientation.VERTICAL, true, false, false)

		val filename = name + ".png"
		ChartUtilities.saveChartAsPNG(new File(filename), barChart, width, height)

		filename
	}
}

class LineChartWriter(name: String, title: String) extends JFreeChartWriter(name, title) {
	val dataset = new DefaultCategoryDataset

	def add[T] (results: Seq[Result[T]]) {
		for(result <- results) {
			val elapsedTime = Statistics.average(Statistics.medianPart(result.elapsedTimes, 0.5))
			val rowName = result.name
			val colName = result.dataset.toString
			dataset.addValue(elapsedTime, rowName, colName)
		}

	}

	def createChart(): String = {
		val chart = createJFreeChart(title, categoryAxisLabel, valueAxisLabel, dataset)

		val filename = name + ".png"
		ChartUtilities.saveChartAsPNG(new File(filename), chart, width, height)

		filename
	}

	def createJFreeChart(title: String, categoryAxisLabel: String, valueAxisLabel: String, dataset: CategoryDataset) = {
	        val categoryAxis = new CategoryAxis(categoryAxisLabel)
        	val valueAxis = new NumberAxis(valueAxisLabel)

        	val renderer = new LineAndShapeRenderer(true, true);
	        val plot = new CategoryPlot(dataset, categoryAxis, valueAxis, renderer);
		plot.setOrientation(PlotOrientation.VERTICAL);
        	val legend = true

		new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, legend);
	}

	def createJFreeChart2(title: String, categoryAxisLabel: String, valueAxisLabel: String, dataset: CategoryDataset) = {
		ChartFactory.createLineChart(title, categoryAxisLabel, valueAxisLabel, dataset, PlotOrientation.VERTICAL, true, false, false)
	}
}

case class Result[T](val name: String, val dataset: T, val elapsedTimes: Seq[Double])

class BenchmarkRunner(val log: Log) {
	val singleRunTimeThreshold = 10000.0
	
	val warmupTimeThreshold = 1000.0
	val warmupCount = 1100
	
	val runTimeThreshold = 1000.0
	val runCount = 100

	val useReferenceRun = false

	def execute(benchmark : Benchmark[Unit]): Result[Unit] = {
		execute(benchmark, ())
	}
	
	def execute[T] (benchmark : Benchmark[T], datasets: Seq[T]): Seq[Result[T]] = {
		val results = new ListBuffer[Result[T]]()
		for(dataset <- datasets) {
			results += execute(benchmark, dataset)
		}		
		results.toList
	}
	
	def execute[T] (benchmark : Benchmark[T], dataset: T): Result[T] = {
		val firstTime = elapsedTime(benchmark, 0, dataset);
		log.warmup(benchmark, dataset, List(firstTime))
		if (firstTime >= singleRunTimeThreshold) {
			log.elapsed(benchmark, dataset, List(firstTime))
			return new Result(benchmark.name, dataset, List(firstTime))
		}
		
		var warmupTimes = execute(benchmark, dataset, warmupCount, warmupTimeThreshold)
		log.warmup(benchmark, dataset, warmupTimes)

		var runTimes = execute(benchmark, dataset, runCount, runTimeThreshold)
		log.elapsed(benchmark, dataset, runTimes)

		new Result(benchmark.name, dataset, runTimes)
	}
	
	def execute[T] (benchmark : Benchmark[T], dataset: T, maxRunCount: Int, timeThreshold: Double): Seq[Double] = {
		val elapsedTimes = new ListBuffer[Double]
		var totalTime = 0.0
		var count = 0
		
		while (count < maxRunCount && totalTime < timeThreshold) {
			val referenceNanos = if (useReferenceRun) elapsedNanosReferenceRun(benchmark, dataset) else 0
			val time = elapsedTime(benchmark, referenceNanos, dataset)
			
			count += 1
			totalTime += time
			elapsedTimes += time
		}
		
		elapsedTimes.toList
	}

	private def elapsedNanosReferenceRun[T] (benchmark : Benchmark[T], dataset: T) = {
		benchmark.setUp(dataset);
		
		val startTime = System.nanoTime;
		benchmark.referenceRun(dataset);
		val endTime = System.nanoTime;
		
		benchmark.tearDown(dataset);
		
		endTime - startTime;
	}
	
	private def elapsedTime[T] (benchmark : Benchmark[T], referenceNanos: Long, dataset: T) = {
		benchmark.setUp(dataset);
		
		val startTime = System.nanoTime;
		benchmark.run(dataset);
		val endTime = System.nanoTime;
		
		benchmark.tearDown(dataset);
		
		(endTime - startTime - referenceNanos) / 1000000.0 / benchmark.elapsedTimeFactor;
	}
}

trait NiceBenchmarkRunner {
	val runner = new BenchmarkRunner(new ConsoleLog(false))

	val chartNamesBuffer = new ListBuffer[String]

	def benchmark(benchmarkName: String)(benchmarkRun: => Unit): Benchmark[Unit] = {
		new SimpleBenchmark() {
			override val name = benchmarkName;

			def run() {
				benchmarkRun
			}
		}
	}	

	def benchmark1[T](benchmarkName: String)(runFunction: (T) => Any): Benchmark[T] = {
		new Benchmark[T]() {
			override val name = benchmarkName;

			def run(dataset: T) {
				runFunction(dataset)
			}
		}
	}	

	def benchmark2[T,V](benchmarkName: String)(setupFunction: (T) => V)(runFunction: (V) => Any) : Benchmark[T] = {
		new FunctionalBenchmark[T,V](benchmarkName) {
			def prepare(dataset: T): V = {
				setupFunction(dataset)
			}

			def run2(dataset: V) {
				runFunction(dataset)
			}
		}
	}	

	def compareChart(name: String, benchmarks: Benchmark[Unit]*) {
		val chartWriter: ChartWriter = new BarChartWriter(name, name)

		val results = benchmarks map (runner.execute(_))
		chartWriter.add(results)
		chartNamesBuffer += chartWriter.createChart()
	}

	def compareChart[T] (name: String, datasets: Seq[T], benchmarks: Benchmark[T]*) {
		val chartWriter = new BarChartWriter(name, name)

		val results = benchmarks map (runner.execute(_, datasets))
		for(result <- results) {
			chartWriter.add(result)
		}
		chartNamesBuffer += chartWriter.createChart()
	}

	def compareChart[T,V] (name: String, dataset: T, benchmarks: Benchmarks[T,V]) {
		compareChart(name, List(dataset), benchmarks)
	}

	def compareChart[T,V] (name: String, datasets: Seq[T], benchmarks: Benchmarks[T,V]) {
		compareChart(name, datasets, benchmarks.benchmarks.toArray: _*)
	}
	
	def datasetChart[T] (name: String, datasetLabel: String, datasets: Seq[T], benchmarks: Benchmark[T]*) {
		val chartWriter = new LineChartWriter(name, name)
		chartWriter.categoryAxisLabel = datasetLabel

		for(benchmark <- benchmarks) {
			val results = runner.execute(benchmark, datasets)
			chartWriter.add(results)
		}
		chartNamesBuffer += chartWriter.createChart()
	}

	def datasetChart[T,V] (name: String, datasetLabel: String, datasets: Seq[T], benchmarks: Benchmarks[T,V]) {
		datasetChart(name, datasetLabel, datasets, benchmarks.benchmarks.toArray: _*)
	}

	def reportAllCharts() {
		val chartNames = chartNamesBuffer.toList
		val report = <html><body>
		Report over {chartNames.length} charts:
		{
			for(chartName <- chartNames) {
				<img href=" { chartName } "/>
			}
		}
		</body></html>

		scala.xml.XML.saveFull("report.html", report, "UTF-8", true, null);
	}
}

class Benchmarks[T,V] {
	var prepareFunction: (T) => V = _

	var benchmarksBuffer = new ListBuffer[Benchmark[T]]

	def prepare(block: (T) => V) {
		prepareFunction = block
	}
	
	def run(name: String)(block: (V) => Any) {
		val benchmark = new FunctionalBenchmark[T,V](name) {
			override def prepare(dataset: T): V = {
				prepareFunction(dataset)
			}
			override def run2(dataset: V) = {
				block(dataset)
			}
		}
		benchmarksBuffer += benchmark
	}

	def benchmarks = {
		benchmarksBuffer.toList
	}
}
