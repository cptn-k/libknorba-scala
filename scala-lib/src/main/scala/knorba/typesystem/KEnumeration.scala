package knorba.typesystem
import java.io.OutputStream


class KEnumeration(private val t: KEnumerationType, val ordinal: Byte)
  extends KValue
{
  if(!t.hasOrdinal(ordinal)) {
    throw new IllegalArgumentException(
      "Ordinal is not defined for enumeration of type " +
        t.name + ": " + ordinal)
  }

  def getOrdinal: Byte = ordinal
  def getLabel: KString = t.getLabelForOrdinal(ordinal).get

  override def getType: KType[_ <: KValue] = t
  override def getTotalSizeInOctets: Long = KEnumerationType.SIZE
  override def writeToBinaryStream(output: OutputStream): Unit =
    output.write(ordinal)

  override def hashCode(): Int = 31 * ordinal

  override def equals(obj: Any): Boolean = obj match {
    case e: KEnumeration =>
      e.t == t && e.ordinal == ordinal
    case _ => false
  }

  override def toString: String = getLabel.toString
}