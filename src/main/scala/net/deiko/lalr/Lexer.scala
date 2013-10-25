package net.deiko.lalr

import scalaz.concurrent.Task
import scalaz.std.option._
import scalaz.std.string._
import scalaz.syntax.foldable._
import scalaz.stream.Process
import Process._

object Lexer {
  def lexer: Process1[Char, Token] =
    receive1(step, emit(EOF)).repeat

  def step(ch: Char): Process1[Char, Token] = ch match {
    case _ if isOp(ch)       => emit(Op(ch))
    case _ if ch.isDigit     => makeLit("", ch)
    case _ if ch.isSpaceChar => halt
    case '('                 => emit(LParens)
    case ')'                 => emit(RParens)
    case '\n'                => emit(EOF)
    case _                   => emit(unexpected(ch))
  }

  def isOp(ch: Char): Boolean = ch match {
    case '+' => true
    case '-' => true
    case '*' => true
    case '/' => true
    case _   => false
  }

  def makeLit(acc: String, ch: Char): Process1[Char, Token] = ch match {
    case _ if ch.isDigit               => receive1[Char, Token](makeLit(acc + ch, _), emit(Lit(acc + ch)) ++ emit(EOF))
    case _ if ch.isSpaceChar           => emit(Lit(acc)) ++ receive1[Char, Token](step, emit(EOF))
    case _ if isOp(ch) && acc.nonEmpty => emit(Lit(acc)) ++ step(ch)
    case _                             => emit(Lit(acc)) ++ step(ch)
  }

  val untermLit = Error("Unterminated numeric value")

  def unexpected(ch: Char, exp: Option[String] = None) = {
    val expStr = exp.foldMap(x => ", expected: " + x)

    Error(s"Invalid character '$ch'$expStr")
  }
}
