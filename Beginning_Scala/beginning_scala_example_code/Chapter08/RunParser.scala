import scala.util.parsing.combinator._

trait RunParser {
  this: RegexParsers =>

  type RootType

  def root: Parser[RootType]

  def run(in: String): ParseResult[RootType] = parseAll(root, in)
}
