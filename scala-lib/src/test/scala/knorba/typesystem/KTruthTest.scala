package knorba.typesystem

import java.io.ByteArrayInputStream

import org.scalatest.funsuite.AnyFunSuite

class KTruthTest extends AnyFunSuite {
  import KTruth._

  private val binary = Array[Byte](1)

  test("operations") {
    assert(X.and(T) == X)
    assert(X.or(T) == T)
    assert(T.and(F) == F)
    assert(T.negate == F)
  }

  test("serialization") {
    assert(T.toBinary.sameElements(binary))
  }

  test("deserialization") {
    val input = new ByteArrayInputStream(binary)
    assert(KType.TRUTH.readFromBinaryStream(input) == T)
  }

  test("castabality") {
    import KType._
    assert(PRIMITIVE_TYPES.filter(TRUTH.isCastableTo(_)) == Seq(TRUTH))
  }
}
