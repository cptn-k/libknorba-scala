package knorba.lang.knoil.syntax

import knorba.lang.common.syntax.Identifier
import net.kfoundation.lang.CodeRange

class DeclerativeStatement(
  range: CodeRange,
  val symbol: Option[Identifier],
  val expr: Expression)
  extends CodeRange(range)
{

}
