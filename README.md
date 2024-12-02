## Advent of Code in Scala 3

This is a project template for the Advent of Code that attempts to automate a few things.

### Requirements

A recent JDK and Scala CLI. I personally like using [SDKMAN](https://sdkman.io/sdks#scalacli).

### Run

Open [https://adventofcode.com](https://adventofcode.com) in your browser, copy the value of the session cookie
token from the headers of any request (just the value after `session=...`).

Export `AOC_AUTH_TOKEN` in your environment.

Fetch a day's input, for instance this year's first day:

```shell

$ scala-cli run . -- fetch 2024 1
```

This should initialize a template for your solution in [src/aoc2024](./src/aoc2024/).

```
$ scala-cli run . -- run 2024 1
```

See [Day01.scala](./src/aoc2024/Day01.scala) for reference.
