package net.deiko.lalr

sealed trait Token

case class Lit(v: String) extends Token
case object LParens extends Token
case object RParens extends Token
case class Op(v: Char) extends Token
case class Error(e: String) extends Token
case object EOF extends Token
