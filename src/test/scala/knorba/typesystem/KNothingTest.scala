package knorba.typesystem

import java.io.ByteArrayInputStream

import org.scalatest.funsuite.AnyFunSuite

class KNothingTest extends AnyFunSuite {
  test("Constructors") {
    assert(KNothing.VALUE.getType == KType.NOTHING)
  }

  test("Serialization") {
    assert(KNothing.VALUE.toBinary.isEmpty)
  }

  test("Deserialization") {
    val input = new ByteArrayInputStream(Array[Byte]())
    assert(KType.NOTHING.readFromBinaryStream(input) == KNothing.VALUE)
  }

  test("Castability") {
    import KType._
    assert(PRIMITIVE_TYPES.filter(NOTHING.isCastableTo(_)) ==
      Seq(NOTHING))
  }
}
