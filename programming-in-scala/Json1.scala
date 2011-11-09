import scala.util.parsing.combinator._
import java.io.FileReader

class Json1 extends JavaTokenParsers {
  def value: Parser[Any] = 
    ( obj
    | arr
    | stringLiteral
    | floatingPointNumber ^^ (_.toDouble)
    | "null"              ^^ (_ => null)
    | "true"              ^^ (_ => true)
    | "false"             ^^ (_ => false)
    )

  def obj: Parser[Map[String, Any]] = "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)

  def arr: Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"

  def member: Parser[(String, Any)] = 
    stringLiteral ~! ":" ~ value ^^ { case name ~ ":" ~ value => (name, value) }
}

object ParseJson1 extends Json1 {
  def main(args: Array[String]) {
    val reader = new FileReader(args(0))
    println(parseAll(value, reader))
  }
}
