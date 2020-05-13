package knorba.lang.knois.syntax

import net.kfoundation.UString

class WQName(val segments: Seq[UString]) {
  def toUString: UString = UString.join(segments, ".")
  override def toString: String = segments.mkString(".")
}
