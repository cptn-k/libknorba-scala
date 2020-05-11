package knorba.lang.knois.syntax

import knorba.lang.knois.lex.KnoisKeyword
import net.kfoundation.UString
import net.kfoundation.lang.CodeRange

class RecordTypeSpec(
  range: CodeRange,
  name: UString,
  val members: Seq[ParamDef])
  extends Spec(range, name)
{
  override def toString: String = {
    val memberStr = members.mkString(", ")
    s"$name is ${KnoisKeyword.RECORD}($memberStr)"
  }
}
