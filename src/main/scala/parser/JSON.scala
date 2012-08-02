package parser
import scala.util.parsing.combinator._

class JSON extends JavaTokenParsers {
  def obj: Parser[Map[String, Any]] =
    "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)
  def arr: Parser[List[Any]] =
    "[" ~> repsep(value, ",") <~ "]"
  def member: Parser[(String, Any)] =
    stringLiteral ~ ":" ~ value ^^ {
      case name ~ ":" ~ value => (name, value)
    }
  def value: Parser[Any] = (
    obj
    | arr
    | stringLiteral
    | floatingPointNumber ^^ (x => x.toDouble)
    | "null" ^^ (_ => null)
    | "true" ^^ (_ => true)
    | "false" ^^ (_ => false))
}

object ParseJSON extends JSON {
  def main(args: Array[String]) {
    val input = """
{
    "glossary": {
        "title": "example glossary",
		"GlossDiv": {
            "title": "S",
			"GlossList": {
                "GlossEntry": {
                    "ID": "SGML",
					"SortAs": "SGML",
					"GlossTerm": "Standard Generalized Markup Language",
					"Acronym": "SGML",
					"Abbrev": "ISO 8879:1986",
					"GlossDef": {
                        "para": "A meta-markup language, used to create markup languages such as DocBook.",
						"GlossSeeAlso": ["GML", "XML"]
                    },
					"GlossSee": "markup"
                }
            }
        }
    }
}
    		"""
    println(parseAll(value, input))
  }
}
