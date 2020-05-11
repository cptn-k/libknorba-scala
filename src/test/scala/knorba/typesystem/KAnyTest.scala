package knorba.typesystem

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import org.scalatest.funsuite.AnyFunSuite

//noinspection EqualityToSameElements
class KAnyTest extends AnyFunSuite {

  private val dummyValue: KValue = KString.of("dummy value")
  private val dummyAny = KAny.of(dummyValue)

  private val dummyBinary = Tools.bytes(0x45, 0x63, 0xab, 0xe7, 0x3b, 0x11, 0x23,
    0x5d, 0x0, 0x0, 0x0, 0xb, 0x64, 0x75, 0x6d, 0x6d, 0x79, 0x20, 0x76, 0x61,
    0x6c, 0x75, 0x65)

  test("Constructors") {
    assert(new KAny(dummyValue).value == dummyValue)
    assert(KAny.of(dummyValue).value == dummyValue)
    assert(KAny.EMPTY.value == KNothing.VALUE)
    assert(KAny.EMPTY.getType == KType.ANY)
    assert(dummyAny.getTotalSizeInOctets == dummyBinary.length)
    assert(KAny.EMPTY.getTotalSizeInOctets == 8)
  }

  test("Serialization") {
    assert(KAny.of(dummyValue).toBinary.sameElements(dummyBinary))

    val output = new ByteArrayOutputStream()
    KAny.of(dummyValue).writeToBinaryStream(output)
    assert(output.toByteArray.sameElements(dummyBinary))
  }

  test("Deserialization") {
    val input = new ByteArrayInputStream(dummyBinary)
    val actual = KType.ANY.readFromBinaryStream(input)
    assert(actual.value == dummyValue)
  }

  test("Equals") {
    assert(KAny.of(KString.of("hello")).equals(KAny.of(KString.of("hello"))))
    assert(!KAny.of(dummyValue).equals(KAny.of(KString.of("hello"))))
    assert(!KAny.of(dummyValue).equals(KAny.EMPTY))
  }

  test("Castability") {
    assert(KType.PRIMITIVE_TYPES.filter(KType.ANY.isCastableTo) ==
      Seq(KType.ANY))

    assert(KType.PRIMITIVE_TYPES.filter(KType.ANY.isAutomaticCastableTo) ==
      Seq(KType.ANY))
  }
}
