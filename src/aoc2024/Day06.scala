package aoc2024

import common.*

class Day06:

  private val sample: String = ???

  def part1(input: Seq[String]): Int = ???

  def part2(input: Seq[String]): Int = ???

  def run(resourcePath: String): Unit =
    assert(part1(Loader.decode(sample)) == 11)
    println("Day06 part 1: " + part1(Loader.load(resourcePath)))
    // assert(part2(Loader.decode(sample)) == 31)
    // println("Day06 part 2: " + part2(Loader.load(resourcePath)))
