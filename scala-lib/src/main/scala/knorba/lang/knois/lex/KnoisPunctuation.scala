package knorba.lang.knois.lex

import net.kfoundation.UChar
import net.kfoundation.lang.lex.PunctuationToken


object KnoisPunctuation extends PunctuationToken.Reader {
  val LEFT_PRAN : UChar = add('(')
  val RIGHT_PRAN: UChar = add(')')
  val DOT       : UChar = add('.')
  val COMMA     : UChar = add(',')
  val COLLEN    : UChar = add(':')
}