package knorba.typesystem
import java.io.InputStream

import scala.collection.mutable


object KEnumerationType {
  class Builder(name: KString) {
    private val values = new mutable.HashMap[Byte, KString]()
    private var maxOrdinal: Byte = 0

    def addMember(ordinal: Byte, label: KString): Builder = {
      values(ordinal) = label
      maxOrdinal = Math.max(maxOrdinal, ordinal+1).toByte
      this
    }

    def addMember(label: KString): Builder = {
      values(maxOrdinal) = label
      maxOrdinal = (maxOrdinal + 1).toByte
      this
    }

    def addAll(labels: Seq[KString]): Builder = {
      labels.foreach(l => addMember(l))
      this
    }

    def build = new KEnumerationType(name, values.toMap)
  }

  val SIZE = 1

  def builder(name: KString): Builder = new Builder(name)
}


class KEnumerationType private (name: KString, values: Map[Byte, KString])
  extends KType[KEnumeration](KEnumerationType.SIZE, name)
{
  private val labelMap = values.map(kv => (kv._2, kv._1))

  def getOrdinalForLabel(label: KString): Option[Byte] = labelMap.get(label)
  def getLabelForOrdinal(ordinal: Byte): Option[KString] = values.get(ordinal)

  def hasLabel(label: KString): Boolean = labelMap.contains(label)
  def hasOrdinal(ordinal: Byte): Boolean = values.contains(ordinal)

  def of(ordinal: Byte): KEnumeration =
    new KEnumeration(this, ordinal)

  def of(label: KString): KEnumeration =
    new KEnumeration(this, labelMap(label))

  override def isCastableTo(that: KType[_]): Boolean = that.equals(this) ||
    (that match {
      case KType.OCTET | KType.INTEGER | KType.LONGINT | KType.REAL => true
      case _ => false
    })

  override def isAutomaticCastableTo(that: KType[_]): Boolean =
    isCastableTo(that)

  override def isPrimitive: Boolean = false
  override def hasConstantSize: Boolean = true

  override def readFromBinaryStream(input: InputStream, tt: TypeTable):
  KEnumeration =
    new KEnumeration(this, NumberIO.readByte(input))

  override def getDefaultValue: KEnumeration = of(values.head._1)

  override def readFromBinaryStream(input: InputStream): KEnumeration = {
    val o = input.read()
    if(o < 0) throw new UnexpectedEndOfStreamException
    new KEnumeration(this, o.toByte)
  }

  override def toString: String = name + " is " +
    values.map(kv => kv._2 + "=" + kv._1).mkString("(", ", ", ")")

  override def getCompatibleTypes: Set[Class[_]] = Set()

  override def valueOf(obj: Object): KEnumeration = obj match {
    case e: KEnumeration if equals(e.getType) => e
    case _ => throw incompatibleException(obj)
  }
}