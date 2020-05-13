package knorba.typesystem

import java.io.OutputStream

abstract class KAbstractRecord(t: KRecordType) extends KValue {
  def get(index: Int): KValue = {
    val info = t.fields(index)
    info.field
      .map(f => t.valueOf(f.get(this)))
      .getOrElse(throw new RuntimeException(
        "Failed to extract value. Field " +
        info.name + " has no reflective information"))
  }

  def get(label: KString): KValue = get(t.indexForLabel(label).getOrElse(
    throw new IllegalArgumentException("No field named " + label)))

  override def getType: KType[_ <: KValue] = t

  override def getTotalSizeInOctets: Long = t.fields
    .indices
    .foldLeft(0L)((s, i) => s + get(i).getTotalSizeInOctets)

  override def writeToBinaryStream(output: OutputStream): Unit = t.fields
    .indices
    .foreach(i => get(i).writeToBinaryStream(output))

  override def toString: String = t.fields
    .indices
    .map(_.toString)
    .mkString("(", ", ", ")")

  override def equals(obj: Any): Boolean = obj match {
    case r: KRecord => t.fields
      .indices
      .forall(i => r.get(i).equals(get(i)))
    case _ => false
  }
}
