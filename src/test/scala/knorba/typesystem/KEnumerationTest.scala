package knorba.typesystem

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import org.scalatest.funsuite.AnyFunSuite


class KEnumerationTest extends AnyFunSuite {
  import KString._
  import Tools.b

  private val testType = KEnumerationType.builder(k"Test")
    .addAll(Seq(k"first", k"second", k"third"))
    .addMember(10, k"tenth")
    .addMember(k"eleventh")
    .build

  test("Constructors") {
    assert(new KEnumeration(testType, b(1)).getOrdinal == 1)
    assertThrows[IllegalArgumentException](new KEnumeration(testType, b(9)))
    assert(testType.of(1.toByte).getOrdinal == 1)
    assert(testType.of(k"eleventh").getOrdinal == 11)
    assert(testType.sizeInOctets == 1)
    assert(testType.of(1.toByte).getTotalSizeInOctets == 1)
  }

  test("Serialization") {
    val output = new ByteArrayOutputStream()
    testType.of(k"second").writeToBinaryStream(output)
    val actual = output.toByteArray
    assert(actual.length == 1)
    assert(actual(0) == 1)
  }

  test("Deserialization") {
    val input = new ByteArrayInputStream(Array[Byte](2))
    assert(testType.readFromBinaryStream(input).getOrdinal == 2)
  }

  test("Ordinal to Label") {
    assert(testType.getLabelForOrdinal(2).contains(k"third"))
    assert(testType.getOrdinalForLabel(k"third").contains(2))

    val v = testType.of(11.toByte)
    assert(v.getLabel == k"eleventh")
  }

  test("Equals") {
    val v1 = testType.of(k"third")
    val v2 = new KEnumeration(testType, b(2))
    assert(v1.equals(v2))
    assert(!v1.equals(testType.of(1.toByte)))
  }

  test("Castability") {
    val types = KType.PRIMITIVE_TYPES
      .filter(testType.isCastableTo)
      .toSet

    assert(types ==
      Set(KType.OCTET, KType.INTEGER, KType.LONGINT, KType.REAL))
  }
}
