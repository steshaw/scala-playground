//
// From http://matt.might.net/articles/implementation-of-m-expression-parser-in-scala-combinators-without-stdlexical-stdtokenparsers/
//

/*** An M-Expressions lexer and parser. ***/

/* The primary puprose of this exercise was to write a lexer and
   parser in Scala without using StandardTokenParsers or
   StdLexical. */

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.syntax._


// Abstract syntax tree nodes.
object MSyntax {
  abstract class MExp

  case class MId(s : String) extends MExp
  case class MApp(f : MExp, args : List[MExp]) extends MExp
}


// Tokens for the Lexer.
trait MTokens extends Tokens {
  case class MSymbol(s : String) extends Token {
    def chars = s
  }

  case class MKeyword(s : String) extends Token {
    def chars = s
  }
}


// The lexer for M-Expressions.
class MLexical extends Lexical with MTokens {

  import scala.util.parsing.input.CharArrayReader.EofCh
  
  def whitespace : Parser[Any] = rep(whitespaceChar)

  def token : Parser[Token] = (
      (rep1(letter)) ^^ { case charList => MSymbol(charList mkString "") }
    | ('[') ^^ { case c => MKeyword(c.toString()) }
    | (']') ^^ { case c => MKeyword(c.toString()) }
    | (';') ^^ { case c => MKeyword(c.toString()) }
    | EofCh ^^^ EOF
  )
}


// The parser for M-Expressions.
object MParser extends TokenParsers {
  type Tokens = MTokens
  val lexical = new MLexical
  
  import lexical.{MSymbol,MKeyword}

  import MSyntax._

  // Required to turn strings into parsers automatically:
  implicit def keyword(chars: String): Parser[String] =
    accept(MKeyword(chars)) ^^ (_.chars)

  
  // Grammar: 
  def prog : Parser[MExp] = phrase(exp) ^^ { case e => e }

  def symbol : Parser[String] =
    elem("identifier", _.isInstanceOf[MSymbol]) ^^ (_.chars)

  def id : Parser[MId] = symbol ^^ { case s => MId(s) } 

  def app : Parser[MApp] = (id ~ "[" ~ (exp) ~ (arg *) ~ "]") ^^
    { case f ~ "[" ~ first  ~ rest ~ "]" => MApp(f,first :: rest) }

  def arg : Parser[MExp] = (";" ~ exp) ^^ { case ";" ~ e => e }

  def exp : Parser[MExp] = (app | id) ^^ { case e => e }


  // External interface:
  def parse (s : String) : MExp = {
    prog(new lexical.Scanner(s)) match {
      case Success(mx, _) => mx

      case Failure(msg,_) =>
       throw new Exception("parse failed: " + msg)

      case Error(msg,_) =>
       throw new Exception("parse error: " + msg)
    }
  }
}


// Driver for the M-Expression parser.
object MExpEngine {

  def stdin : String = {
    val s = new StringBuilder() ;
    var c = System.in.read() 

    while (c != -1) {
      s.append(c.asInstanceOf[Char])
      c = System.in.read()
    }

    s.toString()
  }

  def main(args : Array[String]) {
    println(MParser.parse(stdin))
  }
}

