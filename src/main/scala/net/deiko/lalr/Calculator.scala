package net.deiko.lalr

import scalaz.\/
import scalaz.concurrent.Task
import scalaz.stream._
import Process._

object Calculator {
  val evaluator: Channel[Task, String \/ Mu[AstOp], String \/ Double] = {
    def go(input: String \/ Mu[AstOp]): Task[String \/ Double] = {
      def step(ast: Mu[AstOp]): Task[Double] =
        Task.now {
          ast.fold[Double] {
            case Number(n) => n
            case Mul(l, r) => l * r
            case Div(l, r) => l / r
            case Add(l, r) => l + r
            case Sub(l, r) => l - r
          }
        }

      input.traverse(step)
    }

    await(Task.now[(String \/ Mu[AstOp]) => Task[String \/ Double]](go))(emit).repeat
  }

  def main(args: Array[String]) {
    import scalaz.std.string._
    import scalaz.syntax.foldable._
    import scalaz.concurrent.Task

    val source: Process[Task, Char] = emitAll("1 + 2 * (3 + 5) + 1")
    println((source |> Lexer.lexer |> Parser.parser).through(evaluator).collect.run)
  }
}
