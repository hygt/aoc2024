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
      Either
        .catchNonFatal:
          val Array(a, b) = s.split("\\s+").map(_.toInt)
          (a, b)
        .left
        .map(_.getMessage.nn)

  def part1(pairs: Seq[(Int, Int)]): Int =
    pairs.unzip match
      case (left, right) =>
        left.sorted
          .zip(right.sorted)
          .foldLeft(0):
            case (acc, (x, y)) =>
              acc + (x - y).abs

  def part2(pairs: Seq[(Int, Int)]): Int =
    pairs.unzip match
      case (left, right) =>
        val freq = right.groupMapReduce(identity)(_ => 1)(_ + _)
        left.map(n => n * freq.getOrElse(n, 0)).sum

  def run(resourcePath: String): Unit =
    assert(part1(Loader.decode(sample)) == 11)
    println("Day01 part 1: " + part1(Loader.load(resourcePath)))
    assert(part2(Loader.decode(sample)) == 31)
    println("Day01 part 2: " + part2(Loader.load(resourcePath)))
