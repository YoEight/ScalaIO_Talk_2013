package net.deiko.lalr

sealed trait AstOp[+A]
case class Number(v: Double) extends AstOp[Nothing]
case class Mul[A](l: A, r: A) extends AstOp[A]
case class Div[A](l: A, r: A) extends AstOp[A]
case class Add[A](l: A, r: A) extends AstOp[A]
case class Sub[A](l: A, r: A) extends AstOp[A]

object AstOp {
  import scalaz.Functor

  implicit val astOpFuntor = new Functor[AstOp] {
    def map[A, B](fa: AstOp[A])(f: A => B): AstOp[B] = fa match {
      case Number(v) => Number(v)
      case Mul(l, r) => Mul(f(l), f(r))
      case Div(l, r) => Div(f(l), f(r))
      case Add(l, r) => Add(f(l), f(r))
      case Sub(l, r) => Sub(f(l), f(r))
    }
  }
}
