/*---[KTruth.scala]--------------------------------------------m(._.)m--------*\
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

import java.io.{InputStream, OutputStream}
import java.lang



object KTruth {
  val F: KTruth = KTruth(0, 'F')
  val T: KTruth = KTruth(1, 'T')
  val X: KTruth = KTruth(2, 'X')

  val VALUES: Array[KTruth] = Array(F, T, X)

  val NOT_TABLE: Array[KTruth] = Array(T, F, X)

  val AND_TABLE: Array[Array[KTruth]] = Array(
    Array(F, F, F),
    Array(F, T, X),
    Array(F, X, X))

  val OR_TABLE:Array[Array[KTruth]] = Array(
    Array(F, T, X),
    Array(T, T, T),
    Array(X, T, X))

  val XOR_TABLE:Array[Array[KTruth]] = Array(
    Array(F, T, X),
    Array(T, F, X),
    Array(X, X, X))

  def valueOf(ordinal: Int): KTruth = {
    if(ordinal < 0 || ordinal > 2) {
      throw new IndexOutOfBoundsException(ordinal)
    }
    VALUES(ordinal)
  }

  /** 3-state logical *not* */
  def not(a: KTruth): KTruth = NOT_TABLE(a.ordinal)

  /** 3-state logical *and* */
  def and(a: KTruth, b: KTruth): KTruth = AND_TABLE(a.ordinal)(b.ordinal)

  /** 3-state logical *or* */
  def or(a: KTruth, b: KTruth): KTruth = OR_TABLE(a.ordinal)(b.ordinal)

  /** 3-state logical *exclusive or* */
  def xor(a: KTruth, b: KTruth): KTruth = XOR_TABLE(a.ordinal)(b.ordinal)

  def readFromBinaryStream(input: InputStream): KTruth = {
    val v = input.read()
    if(v == -1) {
      throw new UnexpectedEndOfStreamException()
    }
    if(v > 2) {
      throw new DeserializationException(
        s"Value does not correspond to truth ordinal: $v")
    }
    VALUES(v)
  }

  def of(b: lang.Boolean): KTruth = if(b) T else F
}


/**
 * Wrapper class for KnoRBA 3-state `truth` type. A value of type `truth`
 * can be either `T` (for true), `F` (for false), or `X` (for unknown).
 *
 * KnoRBA employs 3-state logic rather than the common Boolean logic mostly
 * because it simplifies implementation of KnoIL interpreter and makes it
 * easier for a group of agents to engage in collaborative decision making.
 */
case class KTruth private (ordinal: Byte, symbol: Char) extends KValue {
  def and(that: KTruth): KTruth = KTruth.and(this, that)
  def or(that: KTruth): KTruth = KTruth.or(this, that)
  def xor(that: KTruth): KTruth = KTruth.xor(this, that)
  def negate: KTruth = KTruth.not(this)

  override def getType: KType[_ <: KValue] = KType.TRUTH
  override def getTotalSizeInOctets: Long = KType.TRUTH.sizeInOctets
  override def writeToBinaryStream(output: OutputStream): Unit =
    output.write(ordinal)
}
