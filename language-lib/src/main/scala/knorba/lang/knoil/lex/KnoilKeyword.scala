package knorba.lang.knoil.lex

import net.kfoundation.UString
import net.kfoundation.lang.lex.KeywordToken

object KnoilKeyword extends KeywordToken.Reader {
  val TRUTH  : UString = add("truth")
  val OCTET  : UString = add("octet")
  val INTEGER: UString = add("integer")
  val REAL   : UString = add("real")
  val STRING : UString = add("string")
  val RAW    : UString = add("raw")
  val GRID   : UString = add("grid")
}