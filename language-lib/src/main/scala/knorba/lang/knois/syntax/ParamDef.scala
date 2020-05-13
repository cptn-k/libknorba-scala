package knorba.lang.knois.syntax

import knorba.lang.common.syntax.{Identifier, TypeDescriptor}
import net.kfoundation.lang.CodeRange

class ParamDef(
  range: CodeRange,
  val identifier: Identifier,
  val typeDesc: Option[TypeDescriptor])
  extends CodeRange(range)
{
  override def toString: String = identifier.toString +
    typeDesc.map(t => ": " + t.toString).getOrElse("")
}
