/*---[KInteger.scala]------------------------------------------m(._.)m--------*\
 |
 |  Project: KnoRBA Scala Library
 |  Author : Hamed "K" KHANDAN (hamed.khandan@mscp.co)
 |
 |  Copyright (c) 2020, Mindscape K.K.
 |
 |  This file is distributed under the KnoRBA Free Public License. See
 |  LICENSE.TXT for details.
 |
 *//////////////////////////////////////////////////////////////////////////////

package knorba.typesystem

import java.io.OutputStream


object KInteger {
  /** The maximum possible value of KnoRBA `integer`. */
  val MAX_VALUE = new KInteger(2147483647)

  /** The minimum possible value of KnoRBA `integer` */
  val MIN_VALUE = new KInteger(-2147483647)

  val ZERO = new KInteger(0)

  def of(value: Int): KInteger = new KInteger(value)
  def of(octets: Seq[Byte], offset: Int): KInteger =
    of(NumberIO.readIntFromByteArray(octets, offset))
}



/**
 * Wrapper class form KnoRBA `integer` type. A value of type `integer` is a
 * 32-bit (4-octet) 2's complement signed integer between KInteger::MAX_VALUE
 * and KInteger::MIN_VALUE.
 */
class KInteger(private val value: Int) extends KValue {
  def get: Int = value

  override def getType: KType[_ <: KValue] = KType.INTEGER
  override def getTotalSizeInOctets: Long = KType.INTEGER.sizeInOctets
  override def writeToBinaryStream(output: OutputStream): Unit =
    NumberIO.writeInt(value, output)

  override def equals(obj: Any): Boolean = obj match {
    case i: KInteger => i.value == value
    case _ => false
  }
}