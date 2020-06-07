package knorba.lang.knoil.lex

import net.kfoundation.UString
import net.kfoundation.lang.lex.OperatorToken

object KnoilOperator extends OperatorToken.Reader {
  val SET : UString = add("is")
  val EQ  : UString = add("=")
  val GT  : UString = add(">")
  val LT  : UString = add("<")
  val GE  : UString = add("<=")
  val LE  : UString = add(">=")
  val IS_A: UString = add(":")
  val AND : UString = add(",")
  val OR  : UString = add(";")
  val NOT : UString = add("not")
}