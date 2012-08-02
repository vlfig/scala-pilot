package parser
import scala.util.parsing.combinator.JavaTokenParsers

class Arithmetic extends JavaTokenParsers {
  def expr: Parser[Any] = term ~ rep("+" ~ term | "-" ~ term)
  def term: Parser[Any] = factor ~ rep("*" ~ factor | "/" ~ factor)
  def factor: Parser[Any] = floatingPointNumber | "(" ~ expr ~ ")"
}

object ParseExpression extends Arithmetic {
  def main(args: Array[String]) {
    val inputs = List("1+2.75*3", "3750000.0000001/23+15", "67368/45465-(11111871+111754)/15")
    inputs.foreach(i => println(parseAll(expr, i)))
  }
}
