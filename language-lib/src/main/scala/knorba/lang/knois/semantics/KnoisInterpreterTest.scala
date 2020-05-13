package knorba.lang.knois.semantics

import knorba.lang.common.syntax.{CustomTypeDescriptor, Identifier, PrimitiveType, PrimitiveTypeDescriptor}
import knorba.lang.knois.compiler.KnoisScalaWriter
import knorba.lang.knois.syntax._
import knorba.typesystem._
import net.kfoundation.UString
import net.kfoundation.UString._
import net.kfoundation.lang.CodeRange
import org.scalatest.flatspec.AnyFlatSpec

class KnoisInterpreterTest extends AnyFlatSpec {

  val dummyRange = new CodeRange("test", 1, 1, 12)
  val dummyService: UString = U"dummy"

  val service = new Service(dummyRange, new WQName(Seq("a", "b", "c")), Seq(
    new RecordTypeSpec(dummyRange, U"Record1", Seq(
      new ParamDef(dummyRange, id(U"param1"), None),
      new ParamDef(dummyRange, id(U"param2"), toDescriptor(PrimitiveType.INTEGER)),
      new ParamDef(dummyRange, id(U"param3"), toDescriptor(PrimitiveType.NOTHING)))),
    new EnumerationTypeSpec(dummyRange, U"Enum1", Seq(
      id(U"member1"), id(U"member2"), id(U"member3"))),
    new RecordTypeSpec(dummyRange, U"Record2", Seq(
      new ParamDef(dummyRange, id("param4"), toDescriptor(U"Record1")),
      new ParamDef(dummyRange, id("param5"), toDescriptor(U"Enum1"))))))

  private def id(name: UString) = new Identifier(dummyRange, name)
  private def toDescriptor(t: PrimitiveType) =
    Some(new PrimitiveTypeDescriptor(dummyRange, t))
  private def toDescriptor(n: UString) =
    Some(new CustomTypeDescriptor(dummyRange, id(n)))

  "interpreter" should "translate primitive types" in {
    val tt = new TypeTable
    KnoisInterpreter.writeToTable(Seq(service), tt)
    println(tt.toString)
  }

  "compiler" should "generate scala code" in {
    KnoisScalaWriter.writeService(service)
  }

}
