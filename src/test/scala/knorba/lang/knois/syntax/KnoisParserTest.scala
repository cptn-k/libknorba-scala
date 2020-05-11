package knorba.lang.knois.syntax

import net.kfoundation.lang.lex._
import org.scalatest.funsuite.AnyFunSuite



class KnoisParserTest extends AnyFunSuite {

  private val builder = new TokenSeqBuilder().kw("service")
    .id("one").p('.').id("two").p('.').id("three").p(':')
    .id("TestType").kw("is").kw("record").p('(')
      .id("first").p(':').kw("string").p(',')
      .id("second").p(')').p('.')

  test("WQName") {
    val services = new KnoisParser(builder.buildWalker).readServices
    services.foreach(println(_))
    assert(services.length == 1)
    testService(services(0))
  }

  private def testService(s: Service): Unit = {
    assert(s.name.toString == "one.two.three")
    assert(s.statements.length == 1)
  }

}
