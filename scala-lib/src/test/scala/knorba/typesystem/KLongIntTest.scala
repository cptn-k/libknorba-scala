package knorba.typesystem

import java.io.ByteArrayInputStream

import org.scalatest.funsuite.AnyFunSuite

class KLongIntTest extends AnyFunSuite {
  private val value = KLongInt.of(123456789)
  private val binary =Tools.bytes(0x0, 0x0, 0x0, 0x0, 0x7, 0x5b, 0xcd, 0x15)

  test("Constructors") {
    assert(new KLongInt(12345).get == 12345)
    assert(KLongInt.of(12345).get == 12345)
    assert(KLongInt.ZERO.get == 0)
    assert(KLongInt.ZERO.getType == KType.LONGINT)
  }

  test("Serialization") {
    assert(value.toBinary sameElements binary)
  }

  test("Deserialization") {
    val input = new ByteArrayInputStream(binary)
    assert(KType.LONGINT.readFromBinaryStream(input) == value)
  }

  test("Castability") {
    import KType._
    assert(PRIMITIVE_TYPES.filter(LONGINT.isCastableTo(_)).toSet ==
      Set(OCTET, INTEGER, LONGINT, REAL))
    assert(PRIMITIVE_TYPES.filter(LONGINT.isAutomaticCastableTo(_)).toSet ==
      Set(LONGINT, REAL))
  }
}
