
package ch.obermuhlner.scala.microbenchmark1

import scala.util.Random
import scala.collection.mutable.ListBuffer

object ExampleStatic {
	def staticMethod(value: Int): Int = { value + 1 }
}

trait ExampleTrait {
	def traitMethod(value: Int): Int = { value + 1 }
}

class ExampleObject extends ExampleTrait {
	def normalMethod(value: Int): Int = { value + 1 }
}

object BenchmarkConfig {
	val methodLoopCount = 1000
	val loopCount = 1000000
}


object RandomData {
	def list(length: Int): List[Int] = {
		list(new Random, length)
	}

	def list(random: Random, length: Int): List[Int] = {
		val result = new ListBuffer[Int]
		for(i <- 0 to length) {
			result += random.nextInt;
		}

		result.toList
	}
}

object ExampleBenchmarkRunner extends NiceBenchmarkRunner {
	def main(args : Array[String]) {

		compareChart("Calls",
			new SimpleBenchmark("Empty") {
				var result: Int = 0
				def run() {
					for(i <- 0 to BenchmarkConfig.methodLoopCount) {
						result += 1
					}
				}
			},
			new SimpleBenchmark("Method") {
				var result: Int = 0
				def run() {
					for(i <- 0 to BenchmarkConfig.methodLoopCount) {
						result = anotherMethod(result)
					}
				}
				def anotherMethod(result: Int): Int = {
					result + 1
				}
			},
			new SimpleBenchmark("Static") {
				var result: Int = 0
				def run() {
					for(i <- 0 to BenchmarkConfig.methodLoopCount) {
						result = ExampleStatic.staticMethod(result)
					}
				}
			},
			new SimpleBenchmark("Trait") {
				var result: Int = 0
				val x: ExampleTrait = new ExampleObject
				def run() {
					for(i <- 0 to BenchmarkConfig.methodLoopCount) {
						result = x.traitMethod(result)
					}
				}
			},
			new SimpleBenchmark("Instance") {
				var result: Int = 0
				val x: ExampleObject = new ExampleObject
				def run() {
					for(i <- 0 to BenchmarkConfig.methodLoopCount) {
						result = x.normalMethod(result)
					}
				}
			},
			new SimpleBenchmark("Function") {
				var result: Int = 0
				val f = (value: Int) => { value + 1 }
				def run() {
					for(i <- 0 to BenchmarkConfig.methodLoopCount) {
						result = f(result)
					}
				}
			},
			new SimpleBenchmark("Closure") {
				var result: Int = 0
				val incrementValue = 1
				val f = (value: Int) => { value + incrementValue }
				def run() {
					for(i <- 0 to BenchmarkConfig.methodLoopCount) {
						result = f(result)
					}
				}
			}
		)

		compareChart("Loops", 1000000, 
			new Benchmarks[Int,Int] {
				prepare { 
					count => count
				}
				run("empty") { count =>
					// does nothing
				}
				run("for") { count =>
					for(i <- 0 to count) {
					}
				}
				run("while") { count =>
					var i = 0
					while (i < count) {
						i+=1
					}
				}
			}
		)

		compareChart("Cast",
			new SimpleBenchmark("asInstanceOf") {
				var any: Any = _
				var result: String = _
				override def setUp() {
					any = "Hello"
				}
				def run() {
					for(i <- 0 to BenchmarkConfig.methodLoopCount) {
						result = any.asInstanceOf[String]
					}
				}
			},
			new SimpleBenchmark("match") {
				var any: Any = _
				var result: String = _
				override def setUp() {
					any = "Hello"
				}
				def run() {
					for(i <- 0 to BenchmarkConfig.methodLoopCount) {
						result = any match {
							case s: String => s
							case _ => throw new IllegalArgumentException
						}
					}
				}
			}
		)

		datasetChart("Cast2", "Loop Count", 0 until 10000 by 100,
			new Benchmarks[Int,Int] {
				var any: Any = "Hello"
				var result: String = _
				prepare {
					count => count
				}
				run("empty") { count =>
				}
				run("empty loop") { count =>
					for(i <- 0 to count) {
					}
				}
				run("asInstanceOf") { count =>
					for(i <- 0 to count) {
						result = any.asInstanceOf[String]
					}
				}
				run("match case") { count =>
					for(i <- 0 to count) {
						result = any match {
							case s: String => s
							case _ => throw new IllegalArgumentException
						}
					}
				}
			}
		)
		
		datasetChart("List", "N elements", 0 until 100 by 10,
			benchmark2[Int,List[Int]]("Sort") (
				(length) => RandomData.list(length)) (
				(list) => list.sorted
			),
			benchmark2[Int,List[Int]]("Reverse") (
				(length) => RandomData.list(length)) (
				(list) => list.reverse
			),
			benchmark2[Int,List[Int]]("Length") (
				(length) => RandomData.list(length)) (
				(list) => list.length
			)
		)
		
		datasetChart("List2", "N elements", 0 until 100 by 10,
			new Benchmarks[Int,List[Int]] {
				prepare {
					(length) => RandomData.list(length)
				}
				run("sort") {
					(list) => list.sorted
				}
				run("reverse") {
					(list) => list.reverse
				}
				run("length") {
					(list) => list.length
				}
			}
		)

		reportAllCharts()
	}
}
