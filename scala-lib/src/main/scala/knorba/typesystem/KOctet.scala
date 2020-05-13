/*---[KOctet.scala]--------------------------------------------m(._.)m--------*\
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

import net.kfoundation.UString


object KOctet {
  val ZERO: KOctet = new KOctet(0)

  private def parseHex(ch: Int): Byte = (
    if(ch >= 'a' && ch <= 'f') {
      10 + ch - 'a'
    } else if(ch >= 'A' && ch <= 'F') {
      10 + ch - 'A'
    } else if(ch >= '0' && ch <= '9') {
      ch - '0'
    } else {
      0
    }).toByte


  private def parseHex(chars: CharSequence): Byte = (
    (parseHex(chars.charAt(0)) << 4)
      + parseHex(chars.charAt(1))
    ).toByte

  def parseHex(str: UString): KOctet = {
    val chars = str.toUtf8
    new KOctet(
      (parseHex(chars(0) << 4) + parseHex(chars(1))).toByte)
  }

  def of(value: Byte) = new KOctet(value)
}


class KOctet(private val value: Byte) extends KValue {
  def this() = this(0)

  def get: Byte = value

  override def getType: KType[_ <: KValue] = KType.OCTET
  override def getTotalSizeInOctets: Long = KType.OCTET.sizeInOctets
  override def writeToBinaryStream(output: OutputStream): Unit =
    output.write(value)

  override def equals(obj: Any): Boolean = obj match {
    case o: KOctet => o.value == value
    case _ => false
  }

  override def toString: String = value.toString
}