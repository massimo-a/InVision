package parser

trait Token {
  val value: String
}

final case class NumberToken(value: String) extends Token

final case class PropertyToken(value: String) extends Token

final case class ObjectToken(value: String) extends Token

final case class OperatorToken(value: String) extends Token
