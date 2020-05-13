package knorba.typesystem

object Tools {
  def b(i: Int): Byte = (i & 0xff).toByte
  def byteSeq(list: Int*): Seq[Byte] = list.map(_.toByte)
  def bytes(list: Int*): Array[Byte] = byteSeq(list:_*).toArray
  def bytesToString(v: KValue): String = v.toBinary
    .map(b => "0x" + (b & 0xff).toHexString)
    .mkString("(", ", ", ")")
}
