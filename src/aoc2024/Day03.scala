package aoc2024

import cats.syntax.either.*
import common.*

class Day03:

  private val sample1: String = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

  private val sample2: String = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  sealed trait Cmd
  final case class Mul(result: Int) extends Cmd
  sealed trait Switch               extends Cmd
  case object Do                    extends Switch
  case object Dont                  extends Switch

  given Decoder[List[Cmd]] with
    def decode(s: String): Either[String, List[Cmd]] =
      val pattern = """mul\((\d+),(\d+)\)|do\(\)|don't\(\)""".r
      Either
        .catchNonFatal:
          pattern
            .findAllMatchIn(s)
            .map: m =>
              m.matched match
                case "do()"    => Do
                case "don't()" => Dont
                case other =>
                  m.subgroups match
                    case List(a, b) => Mul(a.toInt * b.toInt)
                    case _          => throw new IllegalArgumentException(s"Invalid match: $other")
            .toList
        .leftMap(_.getMessage.nn)

  def part1(input: List[Cmd]): Int =
    input.foldLeft(0): (acc, cmd) =>
      cmd match
        case Mul(m) => acc + m
        case _      => acc

  @annotation.tailrec
  final def part2(input: List[Cmd], enabled: Switch = Do, acc: Int = 0): Int =
    input match
      case Nil          => acc
      case Do :: rest   => part2(rest, Do, acc)
      case Dont :: rest => part2(rest, Dont, acc)
      case Mul(m) :: rest =>
        if enabled == Do then part2(rest, Do, acc + m)
        else part2(rest, Dont, acc)

  def run(resourcePath: String): Unit =
    assert(part1(sample1.decoded) == 161)
    println("Day03 part 1: " + part1(Loader.entire(resourcePath)))
    assert(part2(sample2.decoded) == 48)
    println("Day03 part 2: " + part2(Loader.entire(resourcePath)))
