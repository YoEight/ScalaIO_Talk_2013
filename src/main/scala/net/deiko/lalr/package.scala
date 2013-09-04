package net.deiko

package object lalr {
  import scalaz.Free

  type Ast = Mu[AstOp]
  type LALR[A] = Free[Parser.LalrOp, A]
}
