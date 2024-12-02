package common

object Main:

  private def resourcePath(year: Int, day: Int): String =
    f"$year/$day%02d.txt"

  private def check(year: Int, day: Int): Unit =
    require(2015 <= year && year < 2050, "invalid year")
    require(1 <= day && day <= 25, "invalid day")

  @main def run(args: String*): Unit =
    val cmd = args.applyOrElse(0, "help")
    val year = args.applyOrElse(1, _ => "2024").toIntOption
    val day = args.applyOrElse(2, _ => "1").toIntOption
    (cmd, year, day) match
      case ("fetch", Some(year), Some(day)) =>
        check(year, day)
        if Http.fetch(year, day) then System.exit(0) else System.exit(1)
      case ("run", Some(year), Some(day)) =>
        check(year, day)
        val cls = Class.forName(f"aoc$year.Day$day%02d")
        val run = cls.getMethod("run", classOf[String])
        val obj = cls.getDeclaredConstructor().newInstance()
        val _ = run.invoke(obj, resourcePath(year, day))
      case _ =>
        println("Usage: scala-cli run . -- (fetch|run) [year] [day]")
        System.exit(1)