package aoc2024

import common.*

class Day04:

  private val sample: String =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin

  enum Dir(val dx: Int, val dy: Int):
    case N  extends Dir(0, 1)
    case E  extends Dir(1, 0)
    case S  extends Dir(0, -1)
    case W  extends Dir(-1, 0)
    case NW extends Dir(-1, 1)
    case NE extends Dir(1, 1)
    case SE extends Dir(1, -1)
    case SW extends Dir(-1, -1)

  class Grid(input: Seq[Seq[Char]]):
    require(input.nonEmpty, "grid input should not be empty")
    require(input.size == input.head.length, "grid input should be a square")
    private val m = input.size
    // Part 1 fairly generic
    def search(term: String): Int =
      require(term.nonEmpty, "search term should not be empty")
      val first = term.head
      val found = for
        x <- input.indices
        y <- input.indices
        if input(y)(x) == first
      yield find(x, y, term.toIndexedSeq)
      found.sum

    private def find(x: Int, y: Int, term: Seq[Char]): Int =
      Dir.values
        .map(range(x, y, term.size))
        .filter(_.size == term.size)
        .count(_.map((a, b) => input(b)(a)) == term)

    private def range(x: Int, y: Int, n: Int)(dir: Dir): Seq[(Int, Int)] =
      (0 until n)
        .map(i => (x + i * dir.dx, y + i * dir.dy))
        .filter((x, y) => 0 <= x && x < m && 0 <= y && y < m)

    // Part 2 not very generic, term is always 3 letters MAS
    def searchX: Int =
      val found = for
        x <- input.indices
        y <- input.indices
        if input(y)(x) == 'A'
      yield cross(x, y)
      found.sum

    private def cross(x: Int, y: Int): Int =
      import Dir.*
      if 0 < x && x < m - 1 && 0 < y && y < m - 1 then
        List(NW, NE, SE, SW).map(dir => input(y + dir.dy)(x + dir.dx)) match
          case List('M', 'M', 'S', 'S') => 1
          case List('M', 'S', 'S', 'M') => 1
          case List('S', 'M', 'M', 'S') => 1
          case List('S', 'S', 'M', 'M') => 1
          case _                        => 0
      else 0

  def part1(input: Seq[String]): Int =
    Grid(input.map(_.toIndexedSeq)).search("XMAS")

  def part2(input: Seq[String]): Int =
    Grid(input.map(_.toIndexedSeq)).searchX

  def run(resourcePath: String): Unit =
    assert(part1(sample.linesIterator.toSeq) == 18)
    val res1 = part1(Loader.load(resourcePath)).toString
    println(s"Day04 part 1: $res1")
    assert(part2(sample.linesIterator.toSeq) == 9)
    val res2 = part2(Loader.load(resourcePath)).toString
    println(s"Day04 part 2: $res2")
    // Http.send(2024, 4, 2, res2)
