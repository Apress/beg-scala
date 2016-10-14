import scala.util.parsing.combinator._

object CalcSkel extends JavaTokenParsers with RunParser {
  lazy val sumExpr = multExpr ~ rep("+" ~ multExpr | "-" ~ multExpr)

  lazy val multExpr = factor ~ rep("*" ~ factor | "/" ~ factor)

  lazy val factor: Parser[Any] = floatingPointNumber | "(" ~ sumExpr ~ ")"

  type RootType = Any

  def root = sumExpr
}
