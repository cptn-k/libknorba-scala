package knorba.lang.common.syntax

import knorba.typesystem.{KType, KValue}
import net.kfoundation.UString
import net.kfoundation.lang.CodeRange

import scala.collection.mutable

object PrimitiveType {
  private val VALUES = new mutable.ListBuffer[PrimitiveType]()

  private def add(t: KType[_ <: KValue]): PrimitiveType = {
    val v = new PrimitiveType(t.name.get, t)
    VALUES.append(v)
    v
  }

  val TRUTH  : PrimitiveType = add(KType.TRUTH)
  val OCTET  : PrimitiveType = add(KType.OCTET)
  val INTEGER: PrimitiveType = add(KType.INTEGER)
  val LONGINT: PrimitiveType = add(KType.LONGINT)
  val REAL   : PrimitiveType = add(KType.REAL)
  val STRING : PrimitiveType = add(KType.STRING)
  val RAW    : PrimitiveType = add(KType.RAW)
  val NOTHING: PrimitiveType = add(KType.NOTHING)

  def values(): Seq[PrimitiveType] = VALUES.toSeq
}

class PrimitiveType(val name: UString, val t: KType[_ <: KValue]) {
  override def toString: String = name.toString
}

class PrimitiveTypeDescriptor(
  range: CodeRange,
  val name: PrimitiveType)
  extends TypeDescriptor(range)
{
  override def toString: String = name.toString
}