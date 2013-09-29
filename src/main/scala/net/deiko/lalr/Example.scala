package net.deiko.lalr

import scalaz.concurrent.Task
import scalaz.stream._
import Process._

object Example {
  def main(args: Array[String]) {
    lazy val asyncSrc = curl("http://localhost:9000/arithmetic")
    val simpleSrc: Process[Task, Char] = emitAll("1 + 2 * 3")
    val compiler  = Lexer.lexer |> Parser.parser
    val serialize = Formatter.formatter // |> toBytes[String]
    val exporter  = processes.fileChunkW("export.txt")

    val app = simpleSrc |> compiler |> Calculator.evaluator

    println(app.collect.run)
  }

  def toBytes[A](implicit A: ToBytes[A]): Process1[A, Array[Byte]] =
    processes.lift(A.toBytes)

  def curl(location: String): Process[Task, Char] = {
    import dispatch._, Defaults._
    import scalaz.{-\/, \/-}

    val svc    = dispatch.url(location)
    val future = dispatch.Http(svc OK as.String)
    val task   = Task.async[String] { k =>
      future.onFailure { case e => k(-\/(e)) }
      future.onSuccess { case a => k(\/-(a)) }
    }

    await[Task, String, Char](task)(str => emitAll(str))
  }
}
