package aoc2024

import cats.syntax.either.*
import common.*

class Day01:

  private val sample: String =
    """3   4
      |4   3
      |2   5
      |1   3
      |3   9
      |3   3
      |""".stripMargin

  given Decoder[(Int, Int)] with
    def decode(s: String): Either[String, (Int, Int)] =
      Either.catchNonFatal:
        val Array(a, b) = s.split("\\s+").map(_.toInt)
        (a, b)
      .left.map(_.getMessage.nn)

  def part1(pairs: Seq[(Int, Int)]): Int =
    pairs.map:
      case (a, b) => List(a, b)
    .transpose match
      case Seq(left, right) =>
        left.sorted.zip(right.sorted).map((a, b) => (a - b).abs).sum
      case _ => throw new IllegalArgumentException("Invalid input, expected two columns")

  def part2(pairs: Seq[(Int, Int)]): Int =
    pairs.map:
      case (a, b) => List(a, b)
    .transpose match
      case Seq(left, right) =>
        val freq = right.groupBy(identity).view.mapValues(_.size)
        left.map(n => n * freq.getOrElse(n, 0)).sum
      case _ => throw new IllegalArgumentException("Invalid input, expected two columns")

  def run(resourcePath: String): Unit =
    assert(part1(Loader.decode(sample)) == 11)
    println("Day01 part 1: " + part1(Loader.load(resourcePath)))
    assert(part2(Loader.decode(sample)) == 31)
    println("Day01 part 2: " + part2(Loader.load(resourcePath)))
