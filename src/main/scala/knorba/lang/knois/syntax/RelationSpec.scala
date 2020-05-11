package knorba.lang.knois.syntax

import knorba.lang.knois.lex.KnoisKeyword
import net.kfoundation.UString
import net.kfoundation.lang.CodeRange

class RelationSpec(range: CodeRange, name: UString, val params: Seq[ParamDef])
  extends Spec(range, name)
{
  override def toString: String = {
    val paramsStr = params.mkString(", ")
    s"$name is ${KnoisKeyword.RELATION}($paramsStr)"
  }
}
