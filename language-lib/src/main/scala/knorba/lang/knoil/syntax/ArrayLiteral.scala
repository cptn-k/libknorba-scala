package knorba.lang.knoil.syntax

import net.kfoundation.lang.CodeRange

class ArrayLiteral(
  range: CodeRange,
  val elements: Seq[Expression])
  extends LiteralExpression(range)
{

}
