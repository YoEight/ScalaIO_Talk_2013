package net.deiko.lalr

import scalaz.concurrent.Task
import scalaz.stream._
import Process._

object Example {
  val consoleSrc: Process[Task, Char] = {
    val readLnTask = for {
      _ <- Task.delay(print("""Enter expr ("quit" to exit): """))
      x <- Task.delay(readLine())
    } yield x + "\n"

    def go(input: String): Process[Task, Char] = input match {
      case "quit\n" => halt
      case _        => emitAll(input) ++ await[Task, String, Char](readLnTask)(go)
    }

    await[Task, String, Char](readLnTask)(go)
  }

  val printOut: Sink[Task, Any] = {
    def go(input: Any) =
      Task.now(println(input.toString))

    await(Task.now[Any => Task[Unit]](go))(emit).repeat
  }

  def main(args: Array[String]) {
    lazy val asyncSrc = curl("http://localhost:9000/arithmetic")
    val simpleSrc: Process[Task, Char] = emitAll("1 + 2 * 3")
    val compiler  = Lexer.lexer |> Parser.parser
    val serialize = Formatter.formatter // |> toBytes[String]
    val exporter  = processes.fileChunkW("export.txt")

    val app = consoleSrc |> compiler |> Calculator.evaluator

    app.to(printOut).run.run
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
