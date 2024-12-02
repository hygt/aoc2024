package aoc2024

import common.*
import cats.syntax.either.*

class Day02:

  private val sample: String =
    """7 6 4 2 1
      |1 2 7 8 9
      |9 7 6 2 1
      |1 3 2 4 5
      |8 6 4 4 1
      |1 3 6 7 9""".stripMargin

  given Decoder[Seq[Int]] with
    def decode(line: String): Either[String, Seq[Int]] =
      Either
        .catchNonFatal:
          line.split(' ').map(_.toInt).toSeq
        .leftMap(_.getMessage.nn)

  def part1(input: Seq[Seq[Int]]): Int =
    input.count: report =>
      check(report)

  def part2(input: Seq[Seq[Int]]): Int =
    val (good, bad) = input.partition(check)
    val tolerated = bad.count: report =>
      report.indices.exists: n =>
        val (left, right) = report.splitAt(n)
        check(left ++ right.tail)
    good.size + tolerated

  private def check(report: Seq[Int]): Boolean =
    val levels = report
      .sliding(2)
      .map:
        case Seq(a, b) => a - b
        case _         => 0
      .toSeq // sliding returns an iterator >:(
    levels.forall(d => 1 <= d && d <= 3) || levels.forall(d => -3 <= d && d <= -1)

  def run(resourcePath: String): Unit =
    assert(part1(Loader.decode(sample)) == 2)
    println("Day02 part 1: " + part1(Loader.load(resourcePath)))
    assert(part2(Loader.decode(sample)) == 4)
    println("Day02 part 2: " + part2(Loader.load(resourcePath)))
