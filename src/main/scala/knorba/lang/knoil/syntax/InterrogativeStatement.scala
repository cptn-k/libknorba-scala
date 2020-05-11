package knorba.lang.knoil.syntax

import net.kfoundation.lang.CodeRange

class InterrogativeStatement(
  range: CodeRange,
  val expr: Expression)
  extends Statement(range)
{

}
