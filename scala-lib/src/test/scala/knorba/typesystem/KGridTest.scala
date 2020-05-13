package knorba.typesystem

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import org.scalatest.funsuite.AnyFunSuite

//noinspection EqualityToSameElements
class KGridTest extends AnyFunSuite {
  import KString._

  private val gridType = new KGridType(KType.STRING, 2)
  private val grid =  new KGrid(gridType, 2, 2)
    .set(k"one", 0, 0)
    .set(k"two", 0, 1)
    .set(k"three", 1, 0)
    .set(k"four", 1, 1)

  private val binary = Tools.bytes(0x2, 0x0, 0x0, 0x0, 0x2, 0x0, 0x0, 0x0, 0x2,
    0x0, 0x0, 0x0, 0x3, 0x6f, 0x6e, 0x65, 0x0, 0x0, 0x0, 0x5, 0x74, 0x68,
    0x72, 0x65, 0x65, 0x0, 0x0, 0x0, 0x3, 0x74, 0x77, 0x6f, 0x0, 0x0, 0x0,
    0x4, 0x66, 0x6f, 0x75, 0x72)

  test("Constructors") {
    val g = new KGrid(gridType, 2, 2)
    assert(g.dims == new KGrid.Dims(2, 2))

    assert(Range(0, 2).forall(i =>
      Range(0, 2).forall(j => g.get(i, j) == KString.EMPTY)))

    assert(gridType.empty()
      .dims
      .iterator
      .sameElements(Seq(0, 0)))
  }

  test("Serialization") {
    val output = new ByteArrayOutputStream()
    grid.writeToBinaryStream(output)
    assert(output.toByteArray.sameElements(binary))
    assert(grid.toBinary.sameElements(binary))
  }

  test("Deserialization") {
    val input = new ByteArrayInputStream(binary)
    val actual = gridType.readFromBinaryStream(input)
    println(actual)
    assert(actual == grid)
  }

  test("Castability") {
    assert(!KType.PRIMITIVE_TYPES.exists(gridType.isCastableTo(_)))
    assert(gridType.isCastableTo(KGridType.of(KType.STRING, 2)))
    assert(!gridType.isCastableTo(KGridType.of(KType.STRING, 3)))
    assert(!gridType.isCastableTo(KGridType.of(KType.INTEGER, 2)))
  }
}
