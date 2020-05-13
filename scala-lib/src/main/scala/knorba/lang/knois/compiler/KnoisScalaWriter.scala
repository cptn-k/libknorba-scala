package knorba.lang.knois.compiler

import java.io.{File, FileOutputStream, OutputStream, PrintWriter}

import knorba.lang.common.syntax._
import knorba.lang.knois.compiler.KnoisScalaWriter.{Indent, KTYPE, NO_INDENT}
import knorba.lang.knois.syntax._
import net.kfoundation.UString
import net.kfoundation.UString._


object KnoisScalaWriter {
  private class Indent(val n: Int) {
    private val str = 1.to(n)
      .foldLeft(new StringBuilder)((b, _) => b.append("    "))
      .toString()
    def inc = new Indent(n + 1)
    override def toString: String = str
  }

  private val NO_INDENT = new Indent(0)
  private val KTYPE = U"KType."


  def writeService(service: Service): Unit = {
    val path = new File(service.name.segments.mkString("/"))
    path.mkdirs()
    service.statements.foreach(s => {
      val outputFile = path.getAbsolutePath + "/" + s.name + ".scala"
      val output = new FileOutputStream(outputFile)
      val writer = new KnoisScalaWriter(output)
      writer.writeTypeSpec(Some(service.name.toUString), s)
      writer.close()
      output.close()
    })
  }


}


class KnoisScalaWriter(os: OutputStream) {
  import PrimitiveType._

  private val pw = new PrintWriter(os)

  def close(): Unit = pw.close()

  def writeTypeSpec(service: Option[UString], s: Spec): Unit = {
    service.foreach(n => pw.printf("package %s\n\n", n))
    pw.println("import knorba.typesystem._\n")
    pw.println("import net.kfoundation.UString\n")
    s match {
      case e: EnumerationTypeSpec => writeEnumeration(NO_INDENT, service, e)
      case e: RecordTypeSpec => writeRecord(NO_INDENT, service, e)
    }
  }

  def writeEnumeration(in: Indent, service: Option[UString], spec: EnumerationTypeSpec): Unit =
  {
    writeEnumObject(in, service, spec)
    pw.println()
    writeEnumClass(in, spec.name)
  }

  def writeEnumObject(in: Indent, service: Option[UString], spec: EnumerationTypeSpec): Unit = {
    pw.printf(in + "object %s {\n", spec.name)
    writeEnumType(in.inc, service, spec)
    pw.printf(in + "}\n")
  }

  def writeEnumType(in: Indent, service: Option[UString], spec: EnumerationTypeSpec): Unit = {
    val name = service.map(_ + "." + spec.name).getOrElse(spec.name)
    pw.printf(in + "val TYPE: KEnumerationType = KEnumerationType.builder(KString.of(\"%s\"))\n", name)
    val in2 = in.inc
    pw.printf(in2 + ".addAll(Seq(\n")
    val in3 = in2.inc
    pw.println(spec.members.map(m => in3 + "KString.of(\"" + m.name + "\")")
      .mkString("", ",\n", "))"))
    pw.printf(in2 + ".build\n")
    spec.members
      .indices
      .foreach(i => pw.print("%sval %s = new %s(%d.toByte)\n".format(in,
        spec.members(i).name, spec.name, i)))
  }

  def writeEnumClass(in: Indent, name: UString): Unit = {
    pw.printf(in + "class %s private (ordinal: Byte) extends KEnumeration(%s.TYPE, ordinal) {\n", name, name)
    pw.printf("}\n")
  }

  def writeRecord(in: Indent, service: Option[UString], spec: RecordTypeSpec): Unit = {
    writeRecordObject(in, service, spec)
    pw.println()
    writeRecordClass(in, spec)
  }

  def writeRecordObject(in: Indent, service: Option[UString], spec: RecordTypeSpec): Unit = {
    val name = service.map(_ + "." + spec.name).getOrElse(spec.name)
    pw.printf(in + "object %s {\n", spec.name)
    val in2 = in.inc
    pw.printf(in2 + "val TYPE = KRecordType.builder(KString.of(\"%s\"))\n", name)
    val in3 = in2.inc
    spec.members.foreach(m =>
      pw.printf(in3 + ".add(KString.of(\"%s\"), %s)\n",
        m.identifier.name,
        scalaNameFor(m.typeDesc)))
    pw.printf(in3 + ".build\n")
    pw.printf(in + "}\n")
  }

  def scalaNameFor(typeSpec: Option[TypeDescriptor]): UString = typeSpec match {
    case Some(s: PrimitiveTypeDescriptor) => KTYPE + s.name.name.toUpperCase
    case Some(s: CustomTypeDescriptor) => s.typeName.name + ".TYPE"
    case Some(_: GridTypeDescriptor) => U"KGrid"
    case None => KTYPE + "STRING"
  }

  def valueClassFor(typeSpec: Option[TypeDescriptor]): UString = typeSpec match {
    case Some(s: PrimitiveTypeDescriptor) => s.name match {
      case NOTHING => U"KNothing"
      case TRUTH => U"KTruth"
      case OCTET => U"KOctet"
      case INTEGER => U"KInteger"
      case LONGINT => U"KLongInt"
      case REAL => U"KReal"
      case STRING => U"KString"
      case RAW => U"KRaw"
    }
    case Some(s: CustomTypeDescriptor) => s.typeName.name
    case Some(_: GridTypeDescriptor) => U"KGrid"
    case None => U"KString"
  }

  def nativeClassFor(typeSpec: Option[TypeDescriptor]): UString = typeSpec match {
    case Some(s: PrimitiveTypeDescriptor) => s.name match {
      case NOTHING => U"KNothing"
      case TRUTH => U"Boolean"
      case OCTET => U"Byte"
      case INTEGER => U"Int"
      case LONGINT => U"Long"
      case REAL => U"Double"
      case STRING => U"UString"
      case RAW => U"KRaw"
    }
    case Some(s: CustomTypeDescriptor) => s.typeName.name
    case Some(_: GridTypeDescriptor) => U"KGrid"
    case None => U"UString"
  }

  def writeRecordClass(in: Indent, spec: RecordTypeSpec): Unit = {
    pw.printf(in + "class %s(\n", spec.name)
    writeParams(in.inc, spec.members)
    pw.printf(in + "extends KRecord(%s.TYPE, Seq(\n", spec.name)
    writeFields(in.inc, spec.members)
    pw.printf("{\n")
    writeGetters(in.inc, spec.members)
    pw.printf("}\n")
  }

  def writeParams(in: Indent, members: Seq[ParamDef]): Unit = {
    val str = members.map(d => in +
      "val %s: %s".format(d.identifier.name, nativeClassFor(d.typeDesc)))
      .mkString("", ",\n", ")")
    pw.println(str)
  }

  def writeFields(in: Indent, members: Seq[ParamDef]): Unit = {
    pw.println(members.map(m => fieldToStr(in, m))
      .mkString("", ",\n", "))"))
  }

  def fieldToStr(in: Indent, param: ParamDef): String =
    in.toString + (param.typeDesc match {
      case Some(_: CustomTypeDescriptor) => param.identifier.name
      case _ => scalaNameFor(param.typeDesc) + ".valueOf(" + param.identifier.name + ")"
    })

  def writeGetters(in: Indent, members: Seq[ParamDef]): Unit = {
    members.indices.foreach(i => writeGetter(in, i, members(i)))
  }

  def writeGetter(in: Indent, i: Int, paramDef: ParamDef): Unit =
    pw.print("%sdef get%s: %s = get(%d).%s\n".format(in,
      paramDef.identifier.name.toFirstUpperCase,
      nativeClassFor(paramDef.typeDesc),
      i,
      typeConversion(paramDef.typeDesc)))


  def typeConversion(typeDesc: Option[TypeDescriptor]): String =
    "asInstanceOf[%s]".format(valueClassFor(typeDesc)) + (typeDesc match {
      case Some(_: PrimitiveTypeDescriptor) => ".get"
      case None => ".get"
      case _ => ""
    })
}