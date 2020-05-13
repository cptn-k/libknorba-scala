/*---[KValue.scala]--------------------------------------------m(._.)m--------*\
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

import java.io.{ByteArrayOutputStream, OutputStream}


object KValue {
}


/**
 * Abstract superclass for all KnoRBA type-wrapper classes.
 * Wrapper classes are responsible for storing, managing, serializing,
 * and deseralizaing KnoRBA binary data format.
 */
trait KValue {
  /**
   * Returns the KnoRBA type for the stored value.
   */
  def getType: KType[_ <: KValue]


  /**
   * Returns the size of the stored value when serialized.
   */
  def getTotalSizeInOctets: Long


  /**
   * Serializes the stored value on to the given output stream.
   *
   * @param output The output stream to serialize to.
   */
  def writeToBinaryStream(output: OutputStream): Unit

  def toBinary: Array[Byte] = {
    val output = new ByteArrayOutputStream()
    writeToBinaryStream(output)
    output.toByteArray
  }
}
