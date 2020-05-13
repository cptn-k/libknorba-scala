package knorba.lang.knois.lex

import net.kfoundation.lang.lex.{CodeWalker, IdentifierToken, LexicalError, NumericToken, StringToken, Token, TokenReader}

import scala.collection.mutable.ListBuffer

object Tokenizer {
  private val readers = Seq[TokenReader[_ <: Token[_]]](
    KnoisKeyword,
    IdentifierToken.reader,
    KnoisPunctuation,
    NumericToken.reader,
    StringToken.reader)

  def nextToken(w: CodeWalker): Option[Token[_]] = {
    var token: Option[Token[_]] = None
    readers.find(r => {
      token = r.tryRead(w)
      token.isDefined
    })
    token
  }

  def tokenize(w: CodeWalker): Seq[Token[_]] = {
    val tokens = new ListBuffer[Token[_]]()
    while(w.hasMore) {
      w.skipSpaces()
      val token = nextToken(w)
      if(token.isEmpty) {
        if(w.hasMore) {
          throw w.lexicalErrorAtBegining("Cannot recognize token after " +
            tokens.lastOption.map(_.toString).getOrElse("the beginning"))
        }
      } else {
        tokens.append(token.get)
      }
    }
    tokens.toSeq
  }
}
