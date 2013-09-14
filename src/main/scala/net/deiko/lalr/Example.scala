package net.deiko.lalr

import scalaz.concurrent.Task
import scalaz.stream._
import Process._

object Example {
  def main(args: Array[String]) {
    val source: Process[Task, Char] = emitAll("1 + 2 * (3 + 5) + 1")
    val compile = Lexer.lexer |> Parser.parser
    val compute = source |> compile |> Calculator.evaluator
    val toFile  = (source |> compile |> Formatter.formatter |> toBytes[String]).to(processes.fileChunkW("export.txt"))

    println(compute.collect.run)
    toFile.run.run
  }

  def toBytes[A](implicit A: ToBytes[A]): Process1[A, Array[Byte]] =
    processes.lift(A.toBytes)
}
