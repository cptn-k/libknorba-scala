package knorba.typesystem

import java.io.InputStream

import knorba.typesystem.KString._

object KGridType {
  def of(elementType: KType[_ <: KValue], rank: Byte): KGridType
    = new KGridType(elementType, rank)
}

class KGridType(val elementType: KType[_ <: KValue], val rank: Byte)
  extends KType[KGrid](0, k"grid($rank) of ${elementType.name}")
{
  def empty(): KGrid = new KGrid(this,
    new KGrid.Dims(Range(0, rank).map(_ => 0):_*))

  override def isCastableTo(that: KType[_]): Boolean = that.equals(this)
  override def isAutomaticCastableTo(that: KType[_]): Boolean = isCastableTo(that)
  override def isPrimitive: Boolean = false
  override def hasConstantSize: Boolean = false
  override def getDefaultValue: KGrid = empty()

  override def readFromBinaryStream(input: InputStream, tt: TypeTable): KGrid =
    KGrid.readFromBinaryStream(this, input, tt)

  override def getCompatibleTypes: Set[Class[_]] = Set()

  override def valueOf(obj: Object): KGrid = obj match {
    case g: KGrid if(equals(g.getType)) => g
    case _ => throw incompatibleException(obj)
  }
}