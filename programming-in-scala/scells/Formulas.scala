package org.stairwaybook.scells

import util.parsing.combinator.RegexParsers

trait Formula

case class Coord(row: Int,  column: Int) extends Formula {
  override def toString = ('A' + column).toChar.toString + row
}
case class Range(c1: Coord,  c2: Coord) extends Formula {
  override def toString = c1.toString + ":" + c2.toString
}
case class Number(value: Double) extends Formula {
  override def toString = value.toString
}
case class Textual(value: String) extends Formula {
  override def toString = value
}
case class Application(function: String, arguments: List[Formula]) extends Formula {
  override def toString = function + arguments.mkString("(", ",", ")")
}
object Empty extends Textual("")

object FormulaParsers extends RegexParsers {
  def ident: Parser[String] = """[a-zA-Z_]\w*""".r
  def decimal: Parser[String] = """-?\d+(\.\d*)?""".r
  def cell: Parser[Coord] =
    // TODO: limited to one letter here.
    """[A-Za-z]\d+""".r ^^ { s =>
      val column = s.charAt(0) - 'A'
      val row = s.substring(1).toInt
      Coord(row, column)
    }
  def range: Parser[Range] = cell ~ ":" ~ cell ^^ {
    case c1~":"~c2 => Range(c1, c2)
  }
  def number: Parser[Number] = decimal ^^ (d => Number(d.toDouble))
  def application: Parser[Application] =
    ident ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
      case f~"("~ps~")" => Application(f, ps)
    }

  // TODO: cell should not be allowed here (at top-level).
  def expr: Parser[Formula] = range | cell | number | application

  def textual: Parser[Formula] = """[^=].*""".r ^^ Textual

  def formula: Parser[Formula] = number | textual | "=" ~> expr

  def parse(input: String): Formula =
    parseAll(formula, input) match {
      case Success(e, _) => e
      case f: NoSuccess => Textual("[" + f.msg + "]")
    }
}