package knorba.lang.knois.lex

import net.kfoundation.UString
import net.kfoundation.lang.lex.KeywordToken

object KnoisKeyword extends KeywordToken.Reader {
  val IS         : UString = add("is")
  val OF         : UString = add("of")
  val RELATION   : UString = add("relation")
  val SERVICE    : UString = add("service")
  val TRUTH      : UString = add("truth")
  val OCTET      : UString = add("octet")
  val INTEGER    : UString = add("integer")
  val LONGINT    : UString = add("longint")
  val REAL       : UString = add("real")
  val STRING     : UString = add("string")
  val RAW        : UString = add("raw")
  val NOTHING    : UString = add("nothing")
  val RECORD     : UString = add("record")
  val GRID       : UString = add("grid")
  val ENUMERATION: UString = add("enumeration")
}