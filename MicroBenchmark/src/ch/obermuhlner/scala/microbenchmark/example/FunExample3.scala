package ch.obermuhlner.scala.microbenchmark.example
import ch.obermuhlner.scala.microbenchmark.FunBenchmarks
import ch.obermuhlner.scala.microbenchmark.ImageReport

/**
 * Compares various implementations of a function to test whether an integer value is prime.
 * 
 * See http://primes.utm.edu/glossary/page.php?sort=WheelFactorization
 */

object FunExample3 extends ImageReport {
  def main(args: Array[String]) {
    lineChart("Loops", 0 to 2, 0 to 100000 by 10000,
      new FunBenchmarks[Int, Int] {
        prepare {
          count => count
        }
        run("for loop", "An empty for loop.") { count =>
          for (i <- 0 to count) {
          }
        }
        run("while loop", "A while loop that counts in a variable without returning a result.") { count =>
          var i = 0
          while (i < count) {
            i += 1
          }
        }
      })
    /*
    lineChart("Primes", 0 to 1, 0 to 100000 by 10000,
      new FunBenchmarks[Int, List[Int]] {
        prepare {
          count => (1 to count) toList
        }
        run("primes by wheel 2 spoke 1") { list =>
          list filter { PrimeWheel2.isPrime(_) }
        }
        run("primes by wheel 2 spoke 1 - while loop") { list =>
          list filter { PrimeWheel2_while.isPrime(_) }
        }
        run("primes by wheel 2 spoke 1 with Stream") { list =>
          list filter { PrimeWheel2Stream.isPrime(_) }
        }
        run("primes by wheel 2, 3 spokes 1, 5") { list =>
          list filter { PrimeWheel2_3.isPrime(_) }
        }
      })
	*/
    (1 to 100) filter PrimeWheel2_3.isPrime foreach println
    println("Finished")
  }

  object PrimeWheel2 {
    def isPrime(v: Int): Boolean = {
      if (v <= 1) {
        return false
      }
      if (v == 2) {
        return true;
      }
      if (v % 2 == 0) {
        return false;
      }
      val n = Math.sqrt(v).intValue
      for (i <- 3 to n by 2) {
        if (v % i == 0) {
          return false
        }
      }
      return true;
    }
  }

  object PrimeWheel2_while {
    def isPrime(v: Int): Boolean = {
      if (v <= 1) {
        return false
      }
      if (v == 2) {
        return true;
      }
      if (v % 2 == 0) {
        return false;
      }
      val n = Math.sqrt(v).intValue
      var i = 3
      while (i < n) {
        if (v % i == 0) {
          return false
        }
        i += 2
      }
      return true;
    }
  }

  object PrimeWheel2Stream {
    def isPrime(v: Int): Boolean = {
      val n = Math.sqrt(v)
      (v > 1) && (primes takeWhile { _ <= n } forall { v % _ != 0 })
    }

    val primes = Stream.cons(2, Stream.from(3, 2) filter isPrime)
  }

  object PrimeWheel2_3 {
    def isPrime(v: Int): Boolean = {
      if (v <= 1) {
        return false
      }
      if (v == 2 || v == 3 || v == 5) {
        return true;
      }
      if (v % 2 == 0 || v % 3 == 0 || v % 5 == 0) {
        return false;
      }
      val n = Math.sqrt(v).intValue
      //for (i <- 7 to n by 6) { // with this for loop isPrime() is much slower then with the while loop
      var i = 7;
      while (i <= n) {
        if (v % i == 0) {
          return false
        }
        if (v % (i + 5) == 0) {
          return false
        }
        i += 6
      }
      true;
    }
  }
}
