package net.deiko.lalr

import scalaz.stream._
import Process._

object Calculator {
  val evaluator: Process1[Ast, Double] = {
    def go(ast: Ast): Double =
      ast.fold[Double] {
        case Number(n) => n
        case Mul(l, r) => l * r
        case Div(l, r) => l / r
        case Add(l, r) => l + r
        case Sub(l, r) => l - r
      }

    processes.lift(go)
  }
}
