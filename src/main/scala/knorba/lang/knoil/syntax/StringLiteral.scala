package knorba.lang.knoil.syntax

import net.kfoundation.lang.CodeRange

class StringLiteral(range: CodeRange, val value: String)
  extends LiteralExpression(range)
{

}
