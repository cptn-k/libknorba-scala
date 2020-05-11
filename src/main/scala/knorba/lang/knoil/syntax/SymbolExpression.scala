package knorba.lang.knoil.syntax

import knorba.lang.common.syntax.Identifier
import net.kfoundation.lang.CodeRange

class SymbolExpression(
  range: CodeRange,
  val symbol: Identifier)
  extends LiteralExpression(range)
{

}
