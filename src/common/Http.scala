package common

import java.io.FileWriter
import java.net.http.*
import java.net.URI
import java.nio.file.Paths

object Http:

  private def filePath(day: Int): String = f"$day%02d.txt"

  private def fullPath(year: Int, day: Int): String = f"resources/$year/day$day%02d.txt"

  private def srcPath(year: Int, day: Int): String = f"src/aoc$year/Day$day%02d.scala"

  def fetch(year: Int, day: Int): Boolean =
    sys.env.get("AOC_AUTH_TOKEN") match
      case Some(token) =>
        val request = HttpRequest
          .newBuilder()
          .uri(new URI(s"https://adventofcode.com/$year/day/$day/input"))
          .headers("Cookie", s"session=$token")
          .GET()
          .build()
        val response = HttpClient
          .newBuilder()
          .build()
          .send(request, HttpResponse.BodyHandlers.ofString())
        val output = Paths.get("resources", year.toString, filePath(day)).toFile
        output.getParentFile.mkdirs()
        val writer = new FileWriter(output)
        writer.write(response.body())
        writer.close()
        println(s"Downloaded input for year: $year, day: $day to ${fullPath(year, day)}")
        touch(year, day)
        true
      case None =>
        System.err.print("Please set the AOC_AUTH_TOKEN environment variable. ")
        System.err.println("Get your session token from https://adventofcode.com.")
        System.err.println("In the headers of any request, you will see a `Cookie: session=<token>`.")
        false

  def send(year: Int, day: Int, part: Int, answer: String): Boolean =
    sys.env.get("AOC_AUTH_TOKEN") match
      case Some(token) =>
        val request = HttpRequest
          .newBuilder()
          .uri(new URI(s"https://adventofcode.com/$year/day/$day/answer"))
          .headers("Cookie", s"session=$token")
          .header("Content-Type", "application/x-www-form-urlencoded")
          .POST(HttpRequest.BodyPublishers.ofString(s"level=$part&answer=$answer"))
          .build()
        val response = HttpClient
          .newBuilder()
          .build()
          .send(request, HttpResponse.BodyHandlers.ofString())
        val body = response.body()
        // grep the summary of the response
        val summary = body.split('\n').find(_.startsWith("<article>")).getOrElse("not found")
        println(summary)
        true
      case None =>
        System.err.print("Please set the AOC_AUTH_TOKEN environment variable. ")
        System.err.println("Get your session token from https://adventofcode.com.")
        System.err.println("In the headers of any request, you will see a `Cookie: session=<token>`.")
        false

  private def touch(year: Int, day: Int): Unit =
    val path = Paths.get(srcPath(year, day))
    val file = path.toFile
    if file.exists then println(s"$path already exists, skipping template creation.")
    else
      file.getParentFile.mkdirs()
      val writer = new FileWriter(file)
      writer.write(template(year, day))
      writer.close()
      println(s"You can now implement your solution in $path")

  private def template(year: Int, day: Int): String =
    val packageName = s"aoc$year"
    val className   = f"Day$day%02d"
    s"""
      |package $packageName
      |
      |import common.*
      |
      |class $className:
      |
      |  private val sample: String = ???
      |
      |  def part1(input: Seq[String]): Int = ???
      |
      |  def part2(input: Seq[String]): Int = ???
      |
      |  def run(resourcePath: String): Unit =
      |    assert(part1(Loader.decode(sample)) == 11)
      |    println("$className part 1: " + part1(Loader.load(resourcePath)))
      |    // assert(part2(Loader.decode(sample)) == 31)
      |    // println("$className part 2: " + part2(Loader.load(resourcePath)))
      |
      |""".stripMargin
