package knorba.typesystem

import java.io.OutputStream

object KNothing {
  val VALUE = new KNothing
}

class KNothing private() extends KValue {
  def get: KNothing = KNothing.VALUE
  override def getType: KType[_ <: KValue] = KType.NOTHING
  override def getTotalSizeInOctets: Long = KType.NOTHING.sizeInOctets
  override def writeToBinaryStream(output: OutputStream): Unit = ()
}