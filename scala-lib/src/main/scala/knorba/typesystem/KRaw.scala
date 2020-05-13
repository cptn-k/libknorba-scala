package knorba.typesystem

import java.io.OutputStream


object KRaw {
  val HEADER_SIZE: Int = KType.LONGINT.sizeInOctets
  val EMPTY: KRaw = new KRaw(Seq.empty[Byte])
  def of(octets: Seq[Byte]): KRaw = new KRaw(octets)
}


class KRaw(private val octets: Seq[Byte]) extends KValue {
  def get: Seq[Byte] = octets

  override def getType: KType[_ <: KValue] = KType.RAW

  override def getTotalSizeInOctets: Long = KRaw.HEADER_SIZE + octets.length

  override def writeToBinaryStream(output: OutputStream): Unit = {
    NumberIO.writeInt(octets.length, output)
    val s = octets.length
    var i = 0
    while(i < s) {
      output.write(octets(i))
      i += 1
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case r: KRaw => r.octets == octets
    case _ => false
  }
}
