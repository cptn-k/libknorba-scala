package knorba.lang.common.syntax

import net.kfoundation.UString
import net.kfoundation.lang.CodeRange
import net.kfoundation.lang.lex.IdentifierToken

object Identifier {
  def of(token: IdentifierToken): Identifier =
    new Identifier(new CodeRange(token), token.value)
}

class Identifier(range: CodeRange, val name: UString)
  extends CodeRange(range)
{
  override def toString: String = name.toString
}