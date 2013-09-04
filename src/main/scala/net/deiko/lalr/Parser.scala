package net.deiko.lalr

import scalaz.stream._
import Process._
import scalaz.{\/, -\/, \/-}
import scalaz.Functor
import scalaz.Free
import Free._

object Parser {
  sealed trait LalrOp[+A]
  case class Shift[A](k: Token => A) extends LalrOp[A]
  case class LookAhead[A](k: Token => A) extends LalrOp[A]
  case class Failure(e: String) extends LalrOp[Nothing]

  implicit val lalrFunctor = new Functor[LalrOp] {
    def map[A, B](fa: LalrOp[A])(f: A => B): LalrOp[B] = fa match {
      case Shift(k)     => Shift(f compose k)
      case LookAhead(k) => LookAhead(f compose k)
      case Failure(e)   => Failure(e)
    }
  }

  def parser: Process1[Token, String \/ Ast] =
    makeParser(parse)

  def recv(f: Token => Process1[Token, String \/ Ast]): Process1[Token, String \/ Ast] =
    receive1[Token, String \/ Ast](f, emit(-\/("Imcomplete parse tree")))

  def makeParser(tree: LALR[Ast]): Process1[Token, String \/ Ast] = {
    def go(step: LALR[Ast], ahead: Option[Token]): Process1[Token, String \/ Ast] = {
      def shifting(k: Token => LALR[Ast])(t: Token): Process1[Token, String \/ Ast] =
        go(k(t), None)

      def looking(k: Token => LALR[Ast])(t: Token): Process1[Token, String \/ Ast] =
        go(k(t), Some(t))

      def interpret(instr: LalrOp[LALR[Ast]]): Process1[Token, String \/ Ast] = instr match {
        case Shift(k)     => ahead.fold(recv(shifting(k)))(shifting(k))
        case LookAhead(k) => ahead.fold(recv(looking(k)))(looking(k))
        case Failure(e)   => emit(-\/(e))
      }

      def finish(ast: Ast): Process1[Token, String \/ Ast] =
        emit(\/-(ast))

      step.resume.fold(interpret, finish)
    }

    go(tree, None)
  }

  def shift: LALR[Token] =
    Suspend[LalrOp, Token](Shift(Return(_)))

  def discard: LALR[Unit] =
    shift.map(_ => ())

  def lookAhead: LALR[Token] =
    Suspend[LalrOp, Token](LookAhead(Return(_)))

  def failure[A](e: String): LALR[A] =
    Suspend[LalrOp, A](Failure(e))

  def reduce(ast: Ast): LALR[Ast] =
    Return(ast)

  object When {
    def apply[A](f: Token => LALR[A]): LALR[A] =
      lookAhead.flatMap(f)
  }

  object Factor {
    def unapply(ch: Char): Option[(Ast, Ast) => Ast] = ch match {
      case '*' => Some(Ast.mul(_, _))
      case '/' => Some(Ast.div(_, _))
      case _   => None
    }
  }

  object Term {
    def unapply(ch: Char): Option[(Ast, Ast) => Ast] = ch match {
      case '+' => Some(Ast.add(_, _))
      case '-' => Some(Ast.sub(_, _))
      case _   => None
    }
  }

  def parse: LALR[Ast] = {
    def end: LALR[Unit] =
      When[Unit] {
        case EOF      => discard
        case Error(e) => failure(e)
        case _        => failure("This is not supposed to happen")
      }

    for {
      e <- expr
      _ <- end
    } yield e
  }

  def expr: LALR[Ast] =
    add

  def add: LALR[Ast] =
    looping(mul) {
      case Op(Term(make)) => Some(make)
      case _              => None
    }

  def mul: LALR[Ast] =
    looping(lit) {
      case Op(Factor(make)) => Some(make)
      case _                => None
    }

  def lit: LALR[Ast] =
    When[Ast] {
      case Lit(n)  => shift.flatMap(_ => reduce(Ast.number(n.toDouble)))
      case LParens => parensExpr
      case x       => failure(s"Unexpected token $x")
    }

  def parensExpr: LALR[Ast] = {
    def go =
      When[Unit] {
        case RParens => discard
        case x       => failure(s"Unexpected token $x")
      }

    for {
      _ <- discard
      e <- expr
      _ <- go
    } yield e
  }

  def looping(action: LALR[Ast])(pred: Token => Option[(Ast, Ast) => Ast]): LALR[Ast] = {
    def go(l: Ast): LALR[Ast] =
      When[Ast] { token =>
        pred(token).fold(reduce(l)){ make =>
          for {
            _ <- shift
            r <- looping(action)(pred)
          } yield make(l, r)
        }
      }

    for {
      l <- action
      r <- go(l)
    } yield r
  }
}
