package knorba.lang.knoil.syntax

import net.kfoundation.lang.CodeRange
import net.kfoundation.lang.lex.OperatorToken


class BinaryOperation(
  range: CodeRange,
  operator: OperatorToken,
  operand1: Expression,
  val operand2: Expression)
  extends UnaryOperation(range, operator, operand1)
