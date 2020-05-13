package knorba.lang.knoil.syntax

import knorba.lang.common.syntax.Identifier
import net.kfoundation.lang.CodeRange

class Call(
  range: CodeRange,
  val callee: Identifier,
  val params: Seq[DeclerativeStatement])
  extends Expression(range)
{

}
