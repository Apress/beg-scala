import scala.util.parsing.combinator._

object Calc extends JavaTokenParsers with RunParser {

  lazy val sumExpr = prodExpr ~
  rep("+" ~> prodExpr ^^ (d => (x: Double) => x + d) |
      "-" ~> prodExpr ^^ (d => (x: Double) => x - d)) ^^ {
    case seed ~ fs => fs.foldLeft(seed)((a, f) => f(a))
  }

  lazy val prodExpr = factor ~
  rep("*" ~> factor ^^ (d => (x: Double) => x * d) |
      "/" ~> factor ^^ (d => (x: Double) => x / d)) ^^ {
    case seed ~ fs => fs.foldLeft(seed)((a, f) => f(a))
  }

  lazy val factor: Parser[Double] =
  floatingPointNumber ^^ (_.toDouble) | "(" ~> sumExpr <~ ")"

  type RootType = Double

  def root = sumExpr
}
