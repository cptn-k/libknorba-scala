package knorba.lang.knois.compiler

import java.nio.file.Path

import knorba.lang.knois.lex.Tokenizer
import knorba.lang.knois.syntax.KnoisParser
import net.kfoundation.lang.lex.{CodeWalker, LexicalError}
import net.kfoundation.lang.syntax.{SyntaxError, TokenWalker}

object KnoisScalaCompiler extends App {
  val path = Path.of(args.head)
  val cw = CodeWalker.load(path)
  try {
    val tokens = Tokenizer.tokenize(cw)
    val parser = new KnoisParser(new TokenWalker(path.getFileName.toString, tokens))
    val services = parser.readServices
    services.foreach(KnoisScalaWriter.writeService)
  } catch {
    case e: LexicalError => System.err.println("Lexical Error: " + e.getMessage)
    case e: SyntaxError => System.err.println("Syntax Error: " + e.getMessage)
  }
}
