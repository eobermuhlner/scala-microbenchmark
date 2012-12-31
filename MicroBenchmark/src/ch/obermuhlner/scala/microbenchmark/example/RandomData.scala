package ch.obermuhlner.scala.microbenchmark.example
import scala.util.Random
import scala.collection.mutable.ListBuffer

import java.util.HashMap
import java.util.concurrent.ConcurrentHashMap

object TestData {

  def list(length: Int): List[Int] = {
    list(length, length)
  }

  def list(length: Int, range: Int): List[Int] = {
    list(new Random, length, range)
  }

  def list(random: Random, length: Int, range: Int): List[Int] = {
    val result = new ListBuffer[Int]
    for (i <- 0 until length) {
      result += random.nextInt(range);
    }

    result.toList
  }

  val mapCache = scala.collection.mutable.Map[Int, Map[Int, String]]()

  def map(length: Int): Map[Int, String] = {
    if (!mapCache.contains(length)) {
      mapCache += (length -> mutableMap(length).toMap)
    }

    mapCache(length)
  }

  def mutableMap(length: Int): scala.collection.mutable.Map[Int, String] = {
    val result = scala.collection.mutable.Map[Int, String]()

    for (i <- 0 until length) {
      result += i -> ("X" + i)
    }

    result
  }

  def hashMap(length: Int): HashMap[Int, String] = {
    val result = new HashMap[Int, String]()

    for (i <- 0 until length) {
      result.put(i, "X" + i)
    }

    result
  }

  def concurrentHashMap(length: Int): ConcurrentHashMap[Int, String] = {
    new ConcurrentHashMap[Int, String](hashMap(length));
  }

  def randomStrings(length: Int): Seq[String] = {
    randomStrings(new Random, length);
  }

  def randomStrings(random: Random, length: Int): Seq[String] = {
    var result = List[String]()

    for (i <- 1 to length) {
      result ::= randomString(random)
    }

    result
  }

  def randomString(random: Random): String = {
    val result = new StringBuilder
    for (i <- 0 to 4) {
      result.append(randomCharacter(random, "aeiou"))
      result.append(randomCharacter(random, "bcdfghklmnprstvwxz"))
    }
    result.toString
  }

  def randomCharacter(random: Random, chars: String): Character = {
    chars(random.nextInt(chars.length))
  }
}
