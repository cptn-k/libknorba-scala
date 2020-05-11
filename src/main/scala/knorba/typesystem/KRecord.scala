package knorba.typesystem

import java.io.OutputStream

object KRecord {
}


class KRecord(t: KRecordType, val values: Seq[KValue]) extends KValue {
  {
    val membersType = values.map(_.getType)
    if(!(membersType == t.getSignature)) {
      throw new IllegalArgumentException("Type mismatch. Expected: " +
        t.getSignature + ", Actual: " + membersType)
    }
  }

  def get(index: Int): KValue = values(index)

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