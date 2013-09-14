package net.deiko.lalr

object Example {
  def main(args: Array[String]) {
    import scalaz.concurrent.Task
    import scalaz.stream.Process

    val source: Process[Task, Char] = Process.emitAll("1 + 2 * (3 + 5) + 1")
    val process = source |> Lexer.lexer |> Parser.parser |> Calculator.evaluator

    println(process.collect.run)
  }
}
