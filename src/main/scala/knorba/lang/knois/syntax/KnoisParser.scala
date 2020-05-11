package knorba.lang.knois.syntax

import knorba.lang.common.syntax._
import knorba.lang.knois.lex.{KnoisKeyword, KnoisPunctuation}
import net.kfoundation.UString
import net.kfoundation.lang.CodeRange
import net.kfoundation.lang.lex._
import net.kfoundation.lang.syntax.{SyntaxError, TokenWalker}

import scala.collection.mutable


class KnoisParser(w: TokenWalker) {

  private def typeSpecKeywordExpected: SyntaxError = {
    import KnoisKeyword._
    w.syntaxError(s"$RECORD, $RELATION, or $ENUMERATION")
  }

  def readServices: Seq[Service] =
    LazyList.continually(readService)
      .takeWhile(_.isDefined)
      .flatten
      .force

  def readService: Option[Service] = {
    val start = w.getCursor
    if(!w.isKeyword(KnoisKeyword.SERVICE)) {
      None
    } else {
      w.step()
      val name = readServiceName
      w.expectPunctuation(KnoisPunctuation.COLLEN)
      val specs = readSpecs
      if(specs.isEmpty) {
        throw w.syntaxError("type specification")
      }
      Some(new Service(w.range(start), name, specs))
    }
  }

  def readServiceName: WQName = {
    var hasMore = true
    val segments = new mutable.ArrayBuffer[UString]()
    while (hasMore) {
      segments.append(w.expect(classOf[IdentifierToken]).value)
      hasMore = w.isPunctuation(KnoisPunctuation.DOT)
      if (hasMore) {
        w.step()
      }
    }
    new WQName(segments.toSeq)
  }

  private def readSpecs: Seq[Spec] =
    LazyList.continually(tryReadSpec)
    .takeWhile(_.isDefined)
    .force
    .flatten

  private def tryReadSpec: Option[Spec] =
    w.thisToken match {
      case Some(t: IdentifierToken) =>
        import KnoisKeyword._
        val start = w.getCursor
        w.step()
        val id = t.value
        w.expectKeyword(KnoisKeyword.IS)
        val spec = w.thisToken match {
          case Some(t: KeywordToken) =>
            w.step()
            t.value match {
              case ENUMERATION => readEnumeration(id, start)
              case RECORD => readRecord(id, start)
              case RELATION => readRelation(id, start)
              case _ => throw typeSpecKeywordExpected
            }
          case _ => throw typeSpecKeywordExpected
        }
        w.expectPunctuation(KnoisPunctuation.DOT)
        Some(spec)
      case _ => None
    }

  private def readEnumeration(id: UString, start: Int): EnumerationTypeSpec = {
    w.expectPunctuation(KnoisPunctuation.LEFT_PRAN)
    val members = new mutable.ListBuffer[IdentifierToken]()
    var hasMore = true
    while(hasMore) {
      val member = w.expectIdentifier()
      members.append(member)
      hasMore = w.isPunctuation(KnoisPunctuation.COMMA)
      if(hasMore) {
        w.step()
      }
    }
    w.expectPunctuation(KnoisPunctuation.RIGHT_PRAN)
    new EnumerationTypeSpec(
      w.range(start),
      id,
      members.map(Identifier.of).toSeq)
  }

  private def readRecord(id: UString, begin: Int): RecordTypeSpec = {
    val params = readParams
    new RecordTypeSpec(w.range(begin), id, params)
  }

  private def readRelation(id: UString, begin: Int): RelationSpec = {
    val params = readParams
    new RelationSpec(w.range(begin), id, params)
  }

  private def readParams: Seq[ParamDef] = {
    w.expectPunctuation(KnoisPunctuation.LEFT_PRAN)
    val members = new mutable.ListBuffer[ParamDef]()
    var hasMore = true
    while(hasMore) {
      val member = readParamDef
      members.append(member)
      hasMore = w.isPunctuation(KnoisPunctuation.COMMA)
      if(hasMore) {
        w.step()
      }
    }
    w.expectPunctuation(KnoisPunctuation.RIGHT_PRAN)
    members.toSeq
  }

  private def readParamDef: ParamDef = {
    val begin = w.getCursor
    val name = Identifier.of(w.expectIdentifier())
    if(w.isPunctuation(KnoisPunctuation.COLLEN)) {
      w.step()
      val paramType = readTypeDescriptor()
      new ParamDef(w.range(begin), name, Some(paramType))
    } else {
      new ParamDef(w.range(begin), name, None)
    }
  }

  private def readTypeDescriptor(): TypeDescriptor =
    tryReadPrimitiveTypeDescriptor()
      .orElse(tryReadGridTypeDescriptor())
      .orElse(tryReadCustomTypeDescriptor())
      .getOrElse(throw w.syntaxError("type descriptor"))

  private def tryReadPrimitiveTypeDescriptor(): Option[PrimitiveTypeDescriptor] =
    w.thisToken match {
      case Some(kw: KeywordToken) => PrimitiveType.values()
        .find(_.name.equals(kw.value))
        .map(tt => new PrimitiveTypeDescriptor(w.range(w.getCursor), tt))
        .map(tt => {w.step(); tt})
      case _ => None
    }

  private def tryReadGridTypeDescriptor(): Option[GridTypeDescriptor] =
    w.thisToken match {
      case Some(kw: KeywordToken) => kw.value match {
        case KnoisKeyword.GRID =>
          val begin = w.getCursor
          w.step()
          w.expectPunctuation(KnoisPunctuation.LEFT_PRAN)
          val rank = w.expect(classOf[IntegralToken]).value
          w.expectPunctuation(KnoisPunctuation.RIGHT_PRAN)
          w.expectKeyword(KnoisKeyword.OF)
          val typeId = Identifier.of(w.expect(classOf[IdentifierToken]))
          Some(new GridTypeDescriptor(w.range(begin), rank.toInt, typeId))
        case _ => None
      }
      case _ => None
    }

  private def tryReadCustomTypeDescriptor(): Option[CustomTypeDescriptor] =
    w.thisToken match {
      case Some(t: IdentifierToken) =>
        w.step()
        Some(new CustomTypeDescriptor(new CodeRange(t), Identifier.of(t)))
      case _ => None
    }

}
