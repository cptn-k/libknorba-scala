package knorba.lang.common.syntax

import net.kfoundation.lang.CodeRange

class GridTypeDescriptor(
  range: CodeRange,
  val rank: Int,
  val itemType: Identifier)
  extends TypeDescriptor(range)
{
  override def toString: String = s"grid($rank) of $itemType"
}