package aoc2024

import cats.syntax.either.*
import common.*

class Day05:

  private val rules: String =
    """47|53
    97|13
    97|61
    97|47
    75|29
    61|13
    75|53
    29|13
    97|29
    53|29
    61|53
    97|53
    61|29
    47|13
    75|47
    97|75
    47|61
    75|61
    47|29
    75|13
    53|13""".stripMargin

  private val updates: String =
    """75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47""".stripMargin

  given Decoder[(Int, Int)] with
    def decode(s: String): Either[String, (Int, Int)] =
      Either
        .catchNonFatal:
          val s"$a|$b" = s: @unchecked
          a.toInt -> b.toInt
        .left
        .map(_.getMessage.nn)

  def part(rules: Set[(Int, Int)], updates: Seq[Seq[Int]]): (Int, Int) =
    val (good, bad) = updates.partition(update => update.zip(update.tail).forall(rules))
    val left        = good.map(update => update(update.size / 2)).sum
    val right       = bad.map(update => update.sortWith((a, b) => rules(a, b))(update.size / 2)).sum
    left -> right

  def run(resourcePath: String): Unit =
    val (t1, t2) = part(Loader.decode(rules).toSet, Loader.decode(updates))
    assert(t1 == 143)
    assert(t2 == 123)
    val (r, u)   = Loader.split[(Int, Int), Seq[Int]](resourcePath)
    val (p1, p2) = part(r.toSet, u)
    println(s"Day05 part 1: $p1")
    println(s"Day05 part 2: $p2")
