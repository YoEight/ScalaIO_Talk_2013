package net.deiko.lalr

import scalaz.stream._
import Process._

object Formatter {
  val formatter: Process1[Ast, String] = {
    def go(ast: Ast): String =
      ast.fold[String] {
        case Number(n) => n.toString
        case Mul(l, r) => s"($l * $r)"
        case Div(l, r) => s"($l / $r)"
        case Add(l, r) => s"($l + $r)"
        case Sub(l, r) => s"($l - $r)"
      }

    processes.lift(go)
  }
}
