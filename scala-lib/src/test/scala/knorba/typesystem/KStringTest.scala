package knorba.typesystem

import java.io.ByteArrayInputStream

import org.scalatest.funsuite.AnyFunSuite

class KStringTest extends AnyFunSuite {
  import net.kfoundation.UString._

  private val value = KString.of("Lorem Ipsum")
  private val binary =Tools.bytes(0x0, 0x0, 0x0, 0xb, 0x4c, 0x6f, 0x72, 0x65,
    0x6d, 0x20, 0x49, 0x70, 0x73, 0x75, 0x6d)

  test("Constructors") {
    assert(new KString(U"test").get == U"test")
    assert(new KString("test").get == U"test")
    assert(KString.of(U"test").get == U"test")
    assert(KString.of("test").get == U"test")
    assert(KString.EMPTY.get == U"")
    assert(KString.EMPTY.getType == KType.STRING)
  }

  test("Serialization") {
    assert(value.toBinary.sameElements(binary))
  }

  test("Deserialization") {
    val input = new ByteArrayInputStream(binary)
    assert(KType.STRING.readFromBinaryStream(input) == value)
  }

  test("Castability") {
    import KType._
    assert(PRIMITIVE_TYPES.filter(STRING.isCastableTo(_)).toSet ==
      Set(STRING, RAW))
    assert(PRIMITIVE_TYPES.filter(STRING.isAutomaticCastableTo(_)).toSet ==
      Set(STRING, RAW))
  }
}
