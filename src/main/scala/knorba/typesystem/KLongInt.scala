package knorba.typesystem
import java.io.OutputStream


object KLongInt {
  /** The minimum possible value for `longinteger` type. */
  val MIN_VALUE = new KLongInt(-9223372036854775807L)

  /** The maximum possible value for `longinteger` type. */
  val MAX_VALUE = new KLongInt(9223372036854775807L)

  val ZERO = new KLongInt(0L)

  def of(value: Long) = new KLongInt(value)
  def of(octets: Seq[Byte], offset: Int): KLongInt =
    of(NumberIO.readLongFromByteArray(octets, offset))
}


class KLongInt(private val value: Long) extends KValue {
  def get: Long = value

  override def getType: KType[_ <: KValue] = KType.LONGINT
  override def getTotalSizeInOctets: Long = KType.LONGINT.sizeInOctets
  override def writeToBinaryStream(output: OutputStream): Unit =
    NumberIO.writeLong(value, output)

  override def toString: String = value.toString

  override def equals(obj: Any): Boolean = obj match {
    case i: KLongInt => i.value == value
    case _ => false
  }
}
