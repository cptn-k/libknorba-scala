package knorba.typesystem
import java.io.OutputStream

object KAny {
  val HEADER_SIZE: Int = 8
  val EMPTY = new KAny(KNothing.VALUE)

  def of(value: KValue) = new KAny(value)
}


class KAny(val value: KValue) extends KValue {
  import KAny._

  /**
   * Returns the KnoRBA type for the stored value.
   */
  override def getType: KType[_ <: KValue] = KType.ANY

  /**
   * Returns the size of the stored value when serialized.
   */
  override def getTotalSizeInOctets: Long =
    HEADER_SIZE + value.getTotalSizeInOctets

  /**
   * Serializes the stored value on to the given output stream.
   *
   * @param output The output stream to serialize to.
   */
  override def writeToBinaryStream(output: OutputStream): Unit = {
    value.getType.name.getHashCode.writeToBinaryStream(output)
    value.writeToBinaryStream(output)
  }


  override def equals(obj: Any): Boolean = obj match {
    case a: KAny => a.value.equals(value)
    case _ => false
  }

  override def toString: String = value.toString
}
