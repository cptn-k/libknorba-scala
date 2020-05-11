package knorba.lang.knois.syntax

import knorba.lang.common.syntax.Identifier
import knorba.lang.knois.lex.KnoisKeyword
import net.kfoundation.UString
import net.kfoundation.lang.CodeRange

class EnumerationTypeSpec(
  range: CodeRange,
  name: UString,
  val members: Seq[Identifier])
  extends Spec(range, name)
{
  override def toString: String = {
    val membersStr = members.mkString(", ")
    s"$name is ${KnoisKeyword.ENUMERATION}($membersStr)"
  }
}
