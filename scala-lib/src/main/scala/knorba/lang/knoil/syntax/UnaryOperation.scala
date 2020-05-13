package knorba.lang.knoil.syntax

import net.kfoundation.lang.CodeRange
import net.kfoundation.lang.lex.OperatorToken

class UnaryOperation(
  range: CodeRange,
  val operator: OperatorToken,
  val operand1: Expression)
  extends Expression(range)
{

}
