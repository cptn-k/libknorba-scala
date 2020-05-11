package knorba.typesystem
import java.io.InputStream

import knorba.typesystem.KRecordType.FieldInfo

import scala.collection.mutable


object KRecordType {

  class FieldInfo(val name: KString,
    val t: KType[_ <: KValue],
    val field: Option[java.lang.reflect.Field])

  class Augment(val service: String, val name: Option[String])
    extends scala.annotation.StaticAnnotation
  {
    def this(service: String) = this(service, None)
  }

  class F(val name: Option[String])
    extends scala.annotation.StaticAnnotation
  {
    def this() = this(None)
  }

  class Builder(name: KString) {
    private val members = new mutable.ListBuffer[FieldInfo]()
    def add(name: KString, t: KType[_ <: KValue]): Builder = {
      members.append(new FieldInfo(name, t, None))
      this
    }
    def build() = new KRecordType(name, members.toSeq)
  }

  private val augmentCache = new mutable.HashMap[Class[_], KRecordType]

  def builder(name: KString): Builder = new Builder(name)

  private def toFieldInfo(f: java.lang.reflect.Field): Option[FieldInfo] = ???
//    Option(f.getAnnotation(classOf[F])).map(a => new FieldInfo(
//      KString.of(a.name.getOrElse(f.getName)),
//      KType.of(f.getType)
//        .getOrElse(throw new IllegalArgumentException(
//          "Cannot use type for record field: " + f.getType.getName)),
//      Some(f)))

  def of(cls: Class[_]): KRecordType = ???
//  {
//    val a = cls.getAnnotation(classOf[Augment])
//    if(a == null) {
//      throw new IllegalArgumentException("Cannot infer record type. Class " +
//        cls.getCanonicalName + " does not have Augment annotation")
//    }
//    augmentCache.getOrElseUpdate(cls, {
//      val name = KString.of(a.name.getOrElse(cls.getSimpleName))
//      new KRecordType(name,
//        cls.getDeclaredFields.flatMap(toFieldInfo))
//    })
//  }
}


class KRecordType(name: KString, val fields: Seq[FieldInfo])
  extends KType[KRecord](0, name)
{
  private var default: KRecord = null
  private val signature = fields.map(_.t)
  private val indices = fields.indices
    .map(i => (fields(i).name, i))
    .toMap

  def getSignature: Seq[KType[_ <: KValue]] = signature
  def of(values: KValue*) = new KRecord(this, values)
  def indexForLabel(label: KString): Option[Int] = indices.get(label)
  def typeForLabel(label: KString): Option[KType[_ <: KValue]] =
    indices.get(label).map(i => fields(i).t)
  def fieldForIndex(i: Int): Option[java.lang.reflect.Field] = fields(i).field

  override def isCastableTo(that: KType[_]): Boolean = that.equals(this)
  override def isAutomaticCastableTo(that: KType[_]): Boolean = isCastableTo(that)
  override def isPrimitive: Boolean = false
  override def hasConstantSize: Boolean = false

  override def readFromBinaryStream(input: InputStream, tt: TypeTable): KRecord =
    new KRecord(this,
      fields.map(m => m.t.readFromBinaryStream(input, tt)))

  override def getDefaultValue: KRecord = {
    if(default == null) {
      default = new KRecord(this, fields.map(_.t.getDefaultValue))
    }
    default
  }

  override def equals(obj: Any): Boolean = obj match {
    case r: KRecordType => r.fields == fields
    case _ => false
  }

  override def toString: String = name + " is " +
    fields.map(m => m.name + ": " + m.t.name).mkString("(", ", ", ")")

  override def getCompatibleTypes: Set[Class[_]] = Set()

  override def valueOf(obj: Object): KRecord = obj match {
    case r: KRecord if(equals(r.getType)) => r
    case _ => throw incompatibleException(obj)
  }
}
