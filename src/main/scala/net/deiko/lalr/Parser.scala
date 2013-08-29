package net.deiko.lalr

import scalaz.stream._
import Process._
import scalaz.Functor
import scalaz.Free
import Free._

object Parser {
  type Stack = List[Cell]
  type Production = Stack => (Cell, Stack)
  type Ast = Mu[AstOp]
  type LALR[A] = Free[LalrOp, A]

  sealed trait Cell
  case class AnAst(v: Ast) extends Cell
  case class AToken(v: Token) extends Cell

  sealed trait LalrOp[+A]
  case class Shift[A](v: A) extends LalrOp[A]
  case class Reduce[A](prod: Production, v: A) extends LalrOp[A]
  case class LookAhead[A](pred: Token => Boolean, k: Boolean => A) extends LalrOp[A]
  case class Failure(k: Token => String) extends LalrOp[Nothing]

  implicit val lalrFunctor = new Functor[LalrOp] {
    def map[A, B](fa: LalrOp[A])(f: A => B): LalrOp[B] = fa match {
      case Shift(v)        => Shift(f(v))
      case Reduce(p, v)    => Reduce(p, f(v))
      case LookAhead(p, k) => LookAhead(p, f compose k)
      case Failure(k)      => Failure(k)
    }
  }

  def shift: LALR[Unit] =
    Suspend[LalrOp, Unit](Shift(Return()))

  def reduce(prod: Production): LALR[Unit] =
    Suspend[LalrOp, Unit](Reduce(prod, Return()))

  def lookAhead(pred: Token => Boolean): LALR[Boolean] =
    Suspend[LalrOp, Boolean](LookAhead(pred, Return(_)))

  def failure[A](k: Token => String): LALR[A] =
    Suspend[LalrOp, A](Failure(k))
}
