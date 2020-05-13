package knorba.typesystem

import java.io.OutputStream
import scala.language.implicitConversions


object KReal {
  /** IEEE 754 representation of positive infinity. */
  private val INFINITY: Long = 0x7FF0000000000000L

  /** IEEE 754 representation of not-a-number (NaN). */
  private val NAN: Long = 0x7FFFFFFFFFFFFFFFL

  val ZERO = new KReal(0)

  implicit def of(value: Double) = new KReal(value)
}


class KReal(private val value: Double) extends KValue {
  def get: Double = value

  override def getType: KType[_ <: KValue] = KType.REAL
  override def getTotalSizeInOctets: Long = KType.REAL.sizeInOctets

  override def writeToBinaryStream(output: OutputStream): Unit = {
    if(value.isInfinite) {
      NumberIO.writeLong(KReal.INFINITY, output)
    } else if(value.isNaN) {
      NumberIO.writeLong(KReal.NAN, output)
    } else {
      NumberIO.writeLong(java.lang.Double.doubleToLongBits(value), output)
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case r: KReal => r.value == value
    case _ => false
  }

  override def toString: String = value.toString
}
