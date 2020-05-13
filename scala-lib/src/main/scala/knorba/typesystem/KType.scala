package knorba.typesystem

import java.io.InputStream

import knorba.typesystem.KString._
import knorba.typesystem.KType.IncompatibleTypeException
import net.kfoundation.UString

object KType {
  class IncompatibleTypeException(obj: Object, t: KType[_ <: KValue])
    extends Exception("Cannot convert value " + obj + " to type " + t.name)

  val NOTHING: KType[KNothing] = new KType[KNothing](0, k"nothing") {
    override def isPrimitive: Boolean = true
    override def hasConstantSize: Boolean = true
    override def isCastableTo(that: KType[_]): Boolean = that.equals(this)
    override def isAutomaticCastableTo(that: KType[_]): Boolean = that.equals(this)
    override def getDefaultValue: KNothing = KNothing.VALUE
    override def readFromBinaryStream(input: InputStream, tt: TypeTable): KNothing =
      KNothing.VALUE

    override def getCompatibleTypes: Set[Class[_]] =
      Set(classOf[Void], classOf[Unit], classOf[KNothing])

    override def valueOf(obj: Object): KNothing =
      if(getCompatibleTypes.contains(obj.getClass))
        KNothing.VALUE
      else
        throw incompatibleException(obj)
  }

  val OCTET: KType[KOctet] = new KType[KOctet](1, k"octet") {
    override def isPrimitive: Boolean = true
    override def hasConstantSize: Boolean = true
    override def isCastableTo(that: KType[_]): Boolean = that match {
      case OCTET | INTEGER | LONGINT | REAL => true
      case _ => false
    }
    override def isAutomaticCastableTo(that: KType[_]): Boolean = isCastableTo(that)
    override def getDefaultValue: KOctet = KOctet.ZERO
    override def readFromBinaryStream(input: InputStream, tt: TypeTable): KOctet =
      new KOctet(NumberIO.readByte(input))

    override def getCompatibleTypes: Set[Class[_]] =
      Set(classOf[Byte], classOf[java.lang.Byte], classOf[KOctet])

    override def valueOf(obj: Object): KOctet = obj match {
      case b: java.lang.Byte => KOctet.of(b)
      case o: KOctet => o
      case _ => throw incompatibleException(obj)
    }
  }

  val TRUTH: KType[KTruth] = new KType[KTruth](1, k"truth") {
    override def isPrimitive: Boolean = true
    override def hasConstantSize: Boolean = true
    override def isCastableTo(that: KType[_]): Boolean = that.equals(TRUTH)
    override def isAutomaticCastableTo(that: KType[_]): Boolean = isCastableTo(that)
    override def getDefaultValue: KTruth = KTruth.X
    override def readFromBinaryStream(input: InputStream, tt: TypeTable): KTruth =
      KTruth.readFromBinaryStream(input)

    override def getCompatibleTypes: Set[Class[_]] =
      Set(classOf[Boolean], classOf[java.lang.Boolean], classOf[KTruth])

    override def valueOf(obj: Object): KTruth = obj match {
      case b: java.lang.Boolean => KTruth.of(b)
      case t: KTruth => t
      case _ => throw incompatibleException(obj)
    }
  }

  val INTEGER: KType[KInteger] = new KType[KInteger](4, k"integer") {
    override def isPrimitive: Boolean = true
    override def hasConstantSize: Boolean = true
    override def isCastableTo(that: KType[_]): Boolean = that match {
      case OCTET | INTEGER | LONGINT | REAL => true
      case _ => false
    }
    override def isAutomaticCastableTo(that: KType[_]): Boolean = that match {
      case INTEGER | LONGINT | REAL => true
      case _ => false
    }
    override def getDefaultValue: KInteger = KInteger.ZERO
    override def readFromBinaryStream(input: InputStream, tt: TypeTable): KInteger =
      new KInteger(NumberIO.readInt(input))

    override def getCompatibleTypes: Set[Class[_]] =
      Set(classOf[Int], classOf[Integer], classOf[KInteger])

    override def valueOf(obj: Object): KInteger = obj match {
      case i: Integer => KInteger.of(i)
      case i: KInteger => i
      case _ => throw incompatibleException(obj)
    }
  }

  val LONGINT: KType[KLongInt] = new KType[KLongInt](8, k"longint") {
    override def isPrimitive: Boolean = true
    override def hasConstantSize: Boolean = true
    override def isCastableTo(that: KType[_]): Boolean = that match {
      case OCTET | INTEGER | LONGINT | REAL => true
      case _ => false
    }
    override def isAutomaticCastableTo(that: KType[_]): Boolean = that match {
      case LONGINT | REAL => true
      case _ => false
    }
    override def getDefaultValue: KLongInt = KLongInt.ZERO
    override def readFromBinaryStream(input: InputStream, tt: TypeTable): KLongInt =
      new KLongInt(NumberIO.readLong(input))

    override def getCompatibleTypes: Set[Class[_]] =
      Set(classOf[Long], classOf[java.lang.Long], classOf[KLongInt])

    override def valueOf(obj: Object): KLongInt = obj match {
      case l: java.lang.Long => KLongInt.of(l)
      case l: KLongInt => l
      case _ => throw incompatibleException(obj)
    }
  }

  val REAL: KType[KReal] = new KType[KReal](8, k"real") {
    override def isPrimitive: Boolean = true
    override def hasConstantSize: Boolean = true
    override def isCastableTo(that: KType[_]): Boolean = that match {
      case OCTET | INTEGER | LONGINT | REAL => true
      case _ => false
    }
    override def isAutomaticCastableTo(that: KType[_]): Boolean =
      that.equals(REAL)
    override def getDefaultValue: KReal = KReal.ZERO
    override def readFromBinaryStream(input: InputStream, tt: TypeTable): KReal =
      new KReal(java.lang.Double.longBitsToDouble(
        NumberIO.readLong(input)))

    override def getCompatibleTypes: Set[Class[_]] =
      Set(classOf[Double], classOf[java.lang.Double], classOf[KReal])

    override def valueOf(obj: Object): KReal = obj match {
      case d: java.lang.Double => KReal.of(d)
      case d: KReal => d
      case _ => throw incompatibleException(obj)
    }
  }

  val GUR: KType[KGUR] = new KType[KGUR](16, k"gur") {
    override def isCastableTo(that: KType[_]): Boolean = that.equals(this)
    override def isAutomaticCastableTo(that: KType[_]): Boolean = isCastableTo(that)
    override def isPrimitive: Boolean = true
    override def hasConstantSize: Boolean = true
    override def getDefaultValue: KGUR = KGUR.NULL
    override def readFromBinaryStream(input: InputStream, tt: TypeTable): KGUR =
      new KGUR(
        KGUR.readAppIdFromBinaryStream(input),
        KGUR.readURefFromBinaryStream(input))

    override def getCompatibleTypes: Set[Class[_]] = Set(classOf[KGUR])

    override def valueOf(obj: Object): KGUR = obj match {
      case g: KGUR => g
      case _ => throw incompatibleException(obj)
    }
  }

  val ANY: KType[KAny] = new KType[KAny](0, k"any") {
    override def isCastableTo(that: KType[_]): Boolean = that.equals(ANY)
    override def isAutomaticCastableTo(that: KType[_]): Boolean = isCastableTo(that)
    override def isPrimitive: Boolean = false
    override def hasConstantSize: Boolean = false
    override def getDefaultValue: KAny = KAny.EMPTY
    override def readFromBinaryStream(input: InputStream, tt: TypeTable): KAny = {
      val hash = LONGINT.readFromBinaryStream(input, tt)
      new KAny(tt.get(hash)
        .map(_.readFromBinaryStream(input, tt))
        .getOrElse(throw new TypeTableException(
          "Type for hash not found: " + hash)))
    }

    override def getCompatibleTypes: Set[Class[_]] = Set()

    override def valueOf(obj: Object): KAny = obj match {
      case a: KAny => a
      case _ => throw incompatibleException(obj)
    }
  }

  val RAW: KType[KRaw] = new KType[KRaw](0, k"raw") {
    override def isCastableTo(that: KType[_]): Boolean = that.equals(this)
    override def isAutomaticCastableTo(that: KType[_]): Boolean = isCastableTo(that)
    override def isPrimitive: Boolean = true
    override def hasConstantSize: Boolean = false
    override def getDefaultValue: KRaw = KRaw.EMPTY
    override def readFromBinaryStream(input: InputStream, tt: TypeTable): KRaw = {
      val size: Int = NumberIO.readInt(input)
      val octets: Array[Byte] = new Array[Byte](size.toInt)
      if(input.read(octets) < size) {
        throw new UnsupportedOperationException
      }
      new KRaw(octets.toSeq)
    }

    override def getCompatibleTypes: Set[Class[_]] =
      Set(classOf[KRaw])

    override def valueOf(obj: Object): KRaw = obj match {
      case r: KRaw => r
      case _ => throw incompatibleException(obj)
    }
  }

  val STRING: KType[KString] = new KType[KString](0, k"string") {
    override def isCastableTo(that: KType[_]): Boolean = that match {
      case STRING | RAW => true
      case _ => false
    }
    override def isAutomaticCastableTo(that: KType[_]): Boolean = isCastableTo(that)
    override def isPrimitive: Boolean = true
    override def hasConstantSize: Boolean = false
    override def getDefaultValue: KString = KString.EMPTY
    override def readFromBinaryStream(input: InputStream, tt: TypeTable): KString =
      KString.readFromBinaryStream(input)

    override def getCompatibleTypes: Set[Class[_]] =
      Set(classOf[String], classOf[UString], classOf[KString])

    override def valueOf(obj: Object): KString = obj match {
      case s: String => KString.of(s)
      case s: UString => KString.of(s)
      case s: KString => s
      case _ => throw incompatibleException(obj)
    }
  }

  val PRIMITIVE_TYPES: Seq[KType[_ <: KValue]] = Seq(
    ANY, GUR, INTEGER, LONGINT, NOTHING, OCTET, RAW, REAL, STRING, TRUTH)
}


abstract class KType[T <: KValue](
  val sizeInOctets: Int,
  val name: KString)
{
  protected def incompatibleException(obj: Object) =
    new IncompatibleTypeException(obj, this)

  def isCastableTo(that: KType[_]): Boolean
  def isAutomaticCastableTo(that: KType[_]): Boolean
  def isPrimitive: Boolean
  def hasConstantSize: Boolean
  def readFromBinaryStream(input: InputStream, tt: TypeTable): T
  def getDefaultValue: T
  def getCompatibleTypes: Set[Class[_]]
  def valueOf(obj: Object): T
  def readFromBinaryStream(input: InputStream): T =
    readFromBinaryStream(input, TypeTable.DEFAULT)

  def getHashCode: KLongInt = name.getHashCode

  override def equals(obj: Any): Boolean = obj match {
    case a: KType[_] => a.name.equals(name)
    case _ => false
  }

  override def toString: String = name.toString
}