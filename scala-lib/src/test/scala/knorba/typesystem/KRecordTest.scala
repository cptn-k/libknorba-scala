package knorba.typesystem

import java.io.ByteArrayInputStream
import org.scalatest.funsuite.AnyFunSuite

object KRecordTest {

//
//
//  @KRecordType.Augment("a.b.c")
//  class Record1(
//    val param1: String,
//    val param2: Int,
//    val param3: KNothing)
//    extends KAbstractRecord
//
//
//  @KRecordType.Augment("a.b.c")
//  class Record2(
//    val param4: Record1,
//    val param5: Enum1)
//    extends KAbstractRecord
}


class KRecordTest extends AnyFunSuite {
  import KString._
  import KType._
  import KReal.of

  private val testType = KRecordType.builder(k"StudentGrade")
    .add(k"name", STRING)
    .add(k"grade", REAL)
    .build()

  private val value = testType.of(k"Billy", 91.5)
  private val binary =Tools.bytes(0x0, 0x0, 0x0, 0x5, 0x42, 0x69, 0x6c, 0x6c,
    0x79, 0x40, 0x56, 0xe0, 0x0, 0x0, 0x0, 0x0, 0x0)

  test("Constructors") {
    val values: Seq[KValue] = Seq(k"John", 92.12)
    assert(new KRecord(testType, values).values == values)
    assert(testType.of(values:_*).values == values)
  }

  test("Serialization") {
    assert(value.toBinary.sameElements(binary))
  }

  test("Deserialization") {
    val input = new ByteArrayInputStream(binary)
    assert(testType.readFromBinaryStream(input) == value)
  }

  test("Castability") {
    assert(!KType.PRIMITIVE_TYPES.exists(testType.isCastableTo(_)))
    assert(testType.isCastableTo(testType))
  }

}
