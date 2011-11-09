import scala.util.parsing.combinator._
import java.io.FileReader

class Json extends JavaTokenParsers {
  def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"

  def obj: Parser[Any] = "{" ~ repsep(member, ",") ~ "}"

  def arr: Parser[Any] = "[" ~ repsep(value, ",") ~ "]"

  def member: Parser[Any] = stringLiteral ~ ":" ~ value

}

object ParseJson extends Json {
  def main(args: Array[String]) {
    val reader = new FileReader(args(0))
    println(parseAll(value, reader))
  }
}
