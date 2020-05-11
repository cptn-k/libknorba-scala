package knorba.typesystem

import java.io.ByteArrayInputStream

import org.scalatest.funsuite.AnyFunSuite

class KRawTest extends AnyFunSuite {

  private val testValue = Seq[Byte](1, 2, 3, 4, 5)
  private val serializedValue = Array[Byte](0x0, 0x0, 0x0, 0x5, 0x1, 0x2, 0x3,
    0x4, 0x5)

  test("Constructors") {
    assert(new KRaw(testValue).get == testValue)
    assert(KRaw.of(testValue).get == testValue)
  }

  test("Serialization") {
    assert(KRaw.of(testValue).toBinary.sameElements(serializedValue))
  }

  test("Deserialization") {
    val input = new ByteArrayInputStream(serializedValue)
    assert(KType.RAW.readFromBinaryStream(input) == KRaw.of(testValue))
  }

  test("Castability") {
    assert(KType.PRIMITIVE_TYPES.filter(KType.RAW.isCastableTo(_)) ==
      Seq(KType.RAW))
  }

}
