package knorba.typesystem

import java.io.ByteArrayInputStream

import org.scalatest.funsuite.AnyFunSuite

class KOctetTest extends AnyFunSuite {
  private val value = KOctet.of(123)
  private val binary =Tools.bytes(0x7B)

  test("Constructors") {
    assert(new KOctet(123).get == 123)
    assert(KOctet.of(123).get == 123)
    assert(value.getType == KType.OCTET)
  }

  test("Serialization") {
    assert(value.toBinary sameElements binary)
  }

  test("Deserialization") {
    val input = new ByteArrayInputStream(binary)
    assert(KType.OCTET.readFromBinaryStream(input) == value)
  }

  test("Castability") {
    import KType._
    assert(PRIMITIVE_TYPES.filter(OCTET.isCastableTo(_)).toSet ==
      Set(OCTET, INTEGER, LONGINT, REAL))
    assert(PRIMITIVE_TYPES.filter(OCTET.isAutomaticCastableTo(_)).toSet ==
      Set(OCTET, INTEGER, LONGINT, REAL))
  }
}
