package net.deiko.lalr

import scalaz.stream._
import Process._

object Calculator {
  def main(args: Array[String]) {
    import scalaz.std.string._
    import scalaz.syntax.foldable._
    import scalaz.concurrent.Task

    val source: Process[Task, Char] = emitAll("1 + 2 * 3")
    println((source |> Lexer.lexer |> Parser.parser).collect.run)
  }
}
