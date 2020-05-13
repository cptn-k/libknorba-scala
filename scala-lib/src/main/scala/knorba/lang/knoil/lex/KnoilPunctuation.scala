package knorba.lang.knoil.lex

import net.kfoundation.UChar
import net.kfoundation.lang.lex.PunctuationToken


private object KnoilPunctuation extends PunctuationToken.Reader {
  val LEFT_PRAN    : UChar = add('(')
  val RIGHT_PRAN   : UChar = add(')')
  val DOT          : UChar = add('.')
  val QUESTION_MARK: UChar = add('?')
  val LEFT_BRACE   : UChar = add('[')
  val RIGHT_BRACE  : UChar = add(']')
  val COMMA        : UChar = add(',')
}