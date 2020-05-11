package knorba.typesystem

import java.io.ByteArrayInputStream

import org.scalatest.funsuite.AnyFunSuite

class KIntegerTest extends AnyFunSuite {
  private val value = KInteger.of(1234567)
  private val binary =Tools.bytes(0x0, 0x12, 0xd6, 0x87)

  test("Constructors") {
    assert(new KInteger(12345).get == 12345)
    assert(KInteger.of(12345).get == 12345)
    assert(KInteger.ZERO.get == 0)
    assert(KInteger.ZERO.getType == KType.INTEGER)
  }

  test("Serialization") {
    assert(value.toBinary sameElements binary)
  }

  test("Deserialization") {
    val input = new ByteArrayInputStream(binary)
    assert(KType.INTEGER.readFromBinaryStream(input) == value)
  }

  test("Castability") {
    import KType._
    assert(PRIMITIVE_TYPES.filter(INTEGER.isCastableTo(_)).toSet ==
      Set(OCTET, INTEGER, LONGINT, REAL))
    assert(PRIMITIVE_TYPES.filter(INTEGER.isAutomaticCastableTo(_)).toSet ==
      Set(INTEGER, LONGINT, REAL))
  }
}
