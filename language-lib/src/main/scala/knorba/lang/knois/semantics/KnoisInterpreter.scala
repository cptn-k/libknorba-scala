package knorba.lang.knois.semantics

import knorba.lang.common.syntax._
import knorba.lang.knois.lex.KnoisPunctuation
import knorba.lang.knois.syntax._
import knorba.typesystem._
import net.kfoundation.UString

object KnoisInterpreter {

  def writeToTable(services: Seq[Service], tt: TypeTable): Unit = {
    predefinedTypeNames(services, tt)
    services.foreach(s => writeToTable(s.name.toUString, s.statements, tt))
  }

  def predefinedTypeNames(services: Seq[Service], tt: TypeTable): Unit =
    services.map(s => (s, s.name.toUString))
      .flatMap(p => p._1.statements.map(st => composeName(p._2, st.name)))
      .foreach(tt.predef)

  def writeToTable(serviceName: UString, specs: Seq[Spec], tt: TypeTable): Unit = {
    specs.foreach(s => tt.put(toKType(serviceName, s, tt)))
  }

  def toKType(service: UString, spec: Spec, tt: TypeTable): KType[_ <: KValue] =
    spec match {
      case e: EnumerationTypeSpec => toKEnumerationType(service, e)
      case r: RelationSpec => toKRelationType(service, r)
      case e: RecordTypeSpec => toKRecordType(service, e, tt)
      case _ => throw new RuntimeException(
        "Cannot convert from " + spec.getClass + " to KType")
    }

  def toKEnumerationType(service: UString, spec: EnumerationTypeSpec): KEnumerationType =
    KEnumerationType.builder(composeName(service, spec.name))
      .addAll(spec.members.map(id => new KString(id.name)))
      .build

  def toKRelationType(service: UString, r: RelationSpec): KRelationType =
    new KRelationType(composeName(service, r.name))

  def toKRecordType(service: UString, e: RecordTypeSpec, tt: TypeTable): KRecordType =
    e.members
      .foldLeft(
        KRecordType.builder(composeName(service, e.name)))(
        (b, e) => b.add(
          new KString(e.identifier.name),
          resolveType(service, e.typeDesc, tt)))
      .build()

  def composeName(service: UString, name: UString): KString =
    new KString(service.append(KnoisPunctuation.DOT).append(name))

  def resolveType(service: UString, mt: Option[TypeDescriptor], tt: TypeTable): KType[_ <: KValue] =
    mt match {
      case None => KType.STRING
      case Some(t) => t match {
        case d: PrimitiveTypeDescriptor => d.name.t
        case d: GridTypeDescriptor => new KGridType(
          resolveType(service, d.itemType.name, tt),
          d.rank.toByte)
        case d: CustomTypeDescriptor => resolveType(service, d.typeName.name, tt)
        case _ => throw new RuntimeException(
          "Cannot decipher descriptor of type " + t.getClass)
      }
    }

  def resolveType(service: UString, name: UString, tt: TypeTable): KType[_ <: KValue] =
    PrimitiveType.values()
      .find(_.name.equals(name))
      .map(_.t)
      .orElse(tt.get(new KString(name)))
      .orElse(tt.get(composeName(service, name)))
      .getOrElse(throw new TypeTableException("Type not found: " + name))

}