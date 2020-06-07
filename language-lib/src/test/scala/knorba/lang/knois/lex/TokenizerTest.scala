package knorba.lang.knois.lex

import net.kfoundation.lang.lex.{CodeWalker, Token, TokenSeqBuilder}
import org.scalatest.flatspec.AnyFlatSpec

class TokenizerTest extends AnyFlatSpec {
  val knois: String = """
    |service co.mscp.website.data:
    |    LocalizedProject is record (name, shortDescription, details, gallery: grid(1) of string, picture).
    |    Variation is enumeration (high, low).
    |""".stripMargin


  private val tokens: Seq[Token[_]] = new TokenSeqBuilder()
    .kw("service").id("co").p('.').id("mscp").p('.').id("website").p('.').id("data").p(':')
      .id("LocalizedProject").kw("is").kw("record").p('(')
        .id("name").p(',')
        .id("shortDescription").p(',')
        .id("details").p(',')
        .id("gallery").p(':').kw("grid").p('(').n(1).p(')').kw("of").kw("string").p(',')
        .id("picture").p(')').p('.')
      .id("Variation").kw("is").kw("enumeration").p('(')
        .id("high").p(',').id("low").p(')').p('.')
    .getTokens

  "Tokenizer" should "read knois" in {
    val actual = Tokenizer.tokenize(CodeWalker.of(knois))
    actual.indices.foreach(i => assert(tokens(i).value == actual(i).value))
  }
}
