package net.deiko.lalr

import scalaz.\/
import scalaz.concurrent.Task
import scalaz.stream._
import Process._

object Calculator {
  val evaluator: Channel[Task, String \/ Ast, String \/ Double] = {
    def go(input: String \/ Ast): Task[String \/ Double] = {
      def step(ast: Ast): Task[Double] =
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

    await(Task.now[(String \/ Ast) => Task[String \/ Double]](go))(emit).repeat
  }
}
