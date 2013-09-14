package net.deiko.lalr

import scalaz.\/
import scalaz.concurrent.Task
import scalaz.stream._
import Process._

object Formatter {
  val formatter: Process1[String \/ Ast, String \/ String] = {
    def go(ast: Ast): String =
      ast.fold[String] {
        case Number(n) => n.toString
        case Mul(l, r) => l + " * " + r
        case Div(l, r) => l + " / " + r
        case Add(l, r) => l + " + " + r
        case Sub(l, r) => l + " - " + r
      }

    processes.lift(_.map(go))
  }
}
