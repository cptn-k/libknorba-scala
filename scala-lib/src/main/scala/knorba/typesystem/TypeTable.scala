package knorba.typesystem

import java.io.InputStream

import knorba.typesystem.TypeTable.PlaceHolder

import scala.collection.mutable


object TypeTable {
  val DEFAULT = new TypeTable

  private class PlaceHolder(name: KString) extends KType[KNothing](0, name) {
    override def isCastableTo(that: KType[_]): Boolean = false
    override def isAutomaticCastableTo(that: KType[_]): Boolean = false
    override def isPrimitive: Boolean = false
    override def hasConstantSize: Boolean = true
    override def getDefaultValue: KNothing = KNothing.VALUE
    override def readFromBinaryStream(input: InputStream, tt: TypeTable): KNothing = KNothing.VALUE
    override def getCompatibleTypes: Set[Class[_]] = Set()
    override def valueOf(obj: Object): KNothing = KNothing.VALUE
    override def toString: String = name + "is placeholder"
  }
}


class TypeTable {
  private val lookup = new mutable.HashMap[Long, KType[_ <: KValue]]()

  KType.PRIMITIVE_TYPES.foreach(t => this.put(t))

  def predef(name: KString): Unit =
    lookup.get(name.getHashCode.get) match {
      case None => lookup.put(name.getHashCode.get, new PlaceHolder(name))
      case Some(t: PlaceHolder) => ()
      case _ =>
        throw new TypeTableException(s"Entry for type '$name' already exists")
    }

  def put(t: KType[_ <: KValue]): Unit = lookup.get(t.getHashCode.get) match {
    case None | Some(_: PlaceHolder) => lookup.put(t.getHashCode.get, t)
    case _ => throw new TypeTableException(
      s"Entry for type ${t.name} already exists")
  }

  def get(name: KString): Option[KType[_ <: KValue]] = get(name.getHashCode)

  def get(hash: KLongInt): Option[KType[_ <: KValue]] = lookup.get(hash.get)
    .filter(!_.isInstanceOf[PlaceHolder])

  def isPredefined(hash: KLongInt): Boolean =
    lookup.get(hash.get).exists(_.isInstanceOf[PlaceHolder])

  def isDefinedOrPredefined(hash: KLongInt): Boolean = lookup.contains(hash.get)

  def isDefined(hash: KLongInt): Boolean = get(hash).isDefined

  override def toString: String = lookup.values.map(_ + ".").mkString("\n")
}
