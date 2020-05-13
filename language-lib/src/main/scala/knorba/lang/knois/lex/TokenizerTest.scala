package knorba.lang.knois.lex

import net.kfoundation.lang.lex.CodeWalker
import org.scalatest.flatspec.AnyFlatSpec

class TokenizerTest extends AnyFlatSpec {
  val knois = """
    |service co.mscp.website.data:
    |    LocalizedProject is record (name, shortDescription, details, gallery: grid(1) of string, picture).
    |    Project is record (key, number: integer, isEnabled: truth).
    |    IndexItem is record (key, name, locale).
    |    owner is relation (p: Project, owner: String).
    |    translation is relation (project: Project, locale: String, value: LocalizedProject).
    |""".stripMargin

  "Tokenizer" should "read knois" in {
    Tokenizer.tokenize(CodeWalker.of(knois)).foreach(println(_))
  }
}
