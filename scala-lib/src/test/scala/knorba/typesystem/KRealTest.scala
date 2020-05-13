package knorba.typesystem

import java.io.ByteArrayInputStream

import org.scalatest.funsuite.AnyFunSuite

class KRealTest extends AnyFunSuite {
  private val value = KReal.of(1234567.1234)
  private val binary =Tools.bytes(0x41, 0x32, 0xd6, 0x87, 0x1f, 0x97, 0x24, 0x74)

  test("Constructors") {
    assert(new KReal(123.45).get == 123.45)
    assert(KReal.of(123.45).get == 123.45)
    assert(KReal.ZERO.get == 0)
    assert(KReal.ZERO.getType == KType.REAL)
  }

  test("Serialization") {
    assert(value.toBinary.sameElements(binary))
  }

  test("Deserialization") {
    val input = new ByteArrayInputStream(binary)
    assert(KType.REAL.readFromBinaryStream(input) == value)
  }

  test("Castability") {
    import KType._
    assert(PRIMITIVE_TYPES.filter(REAL.isCastableTo(_)).toSet ==
      Set(OCTET, INTEGER, LONGINT, REAL))
    assert(PRIMITIVE_TYPES.filter(REAL.isAutomaticCastableTo(_)).toSet ==
      Set(REAL))
  }
}
