package knorba.lang.common.syntax

import net.kfoundation.lang.CodeRange

class CustomTypeDescriptor(
  range: CodeRange,
  val typeName: Identifier)
  extends TypeDescriptor(range)
{
  override def toString: String = typeName.toString
}