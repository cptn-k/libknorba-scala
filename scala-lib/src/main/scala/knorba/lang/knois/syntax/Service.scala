package knorba.lang.knois.syntax

import knorba.lang.knois.lex.KnoisKeyword
import net.kfoundation.lang.CodeRange

class Service(
  range: CodeRange,
  val name: WQName,
  val statements: Seq[Spec])
  extends CodeRange(range)
{
  override def toString: String = {
    val statementsStr = statements.mkString("\n  ")
    s"${KnoisKeyword.SERVICE} $name:\n  $statementsStr"
  }
}