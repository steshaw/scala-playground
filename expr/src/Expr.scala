//
// Originally from http://jim-mcbeath.blogspot.com/2008/09/scala-parser-combinators.html
//

sealed abstract class Expr {
  def eval():Double
}

final case class EConst(value:Double) extends Expr {
  def eval():Double = value
}

final case class EAdd(left:Expr, right:Expr) extends Expr {
  def eval():Double = left.eval + right.eval
}
final case class ESub(left:Expr, right:Expr) extends Expr {
  def eval():Double = left.eval - right.eval
}
final case class EMul(left:Expr, right:Expr) extends Expr {
  def eval():Double = left.eval * right.eval
}
final case class EDiv(left:Expr, right:Expr) extends Expr {
  def eval():Double = left.eval / right.eval
}
final case class EUnaryMinus(e:Expr) extends Expr {
  def eval():Double = -e.eval
}

//case class ESum(a:List[Expr]) extends Expr {
//  def eval():Double =a.foldLeft(0.0)(_ + _.eval)
//}

import scala.util.parsing.combinator.syntactical._

object ExprParser extends StandardTokenParsers {
  override val lexical = new ExprLexical

  lexical.delimiters ++= List("+", "-", "*", "/", "(", ")")

  def parse(s:String) = {
    val tokens = new lexical.Scanner(s)
    phrase(expr)(tokens)
  }

  def expr = (binary(minPrec) | value)

//  def sum = value ~ "+" ~ value ^^ {
//    case left ~ "+" ~ right => EAdd(left, right)
//  }

//  def sumList = rep1sep(value, "+") ^^ {
//    a:List[Expr] => {
//      def makeExpr(list:List[Expr]):Expr = {
//        def xs = list.reverse
//        println("list = " + list)
//        println("xs = " + xs)
//        if (xs.length <= 1) error("foo needs list of 2 or more")
//        if (xs.length == 2) EAdd(xs.first, xs.tail.first)
//        else EAdd(xs.first, makeExpr(xs.tail))
//      }
//       makeExpr(a)
//    }
//  }

//  def sum = repsep(value, "+") ^^ { a:List[Expr] => ESum(a) }

  def sum = product * (
      "+" ^^^ { (a:Expr, b:Expr) => EAdd(a,b) }
    | "-" ^^^ { (a:Expr, b:Expr) => ESub(a,b) }
  )

  def binaryOp(level:Int):Parser[((Expr,Expr)=>Expr)] = {
    level match {
      case 1 =>
        "+" ^^^ { (a:Expr, b:Expr) => EAdd(a,b) } |
        "-" ^^^ { (a:Expr, b:Expr) => ESub(a,b) }

      case 2 =>
        "*" ^^^ { (a:Expr, b:Expr) => EMul(a,b) } |
        "/" ^^^ { (a:Expr, b:Expr) => EDiv(a,b) }

      case _ => error("bad precendence level: " + level)
    }
  }

  val minPrec = 1
  val maxPrec = 2

  def binary(level:Int):Parser[Expr] =
    if (level > maxPrec) term
    else binary(level + 1) * binaryOp(level)

  def product = term * (
    "*" ^^^ { (a:Expr, b:Expr) => EMul(a,b) }
  )

  def parens:Parser[Expr] = "(" ~> expr <~ ")"

  def term = (value | parens | unaryMinus )

  def value = numericLit ^^ {
    s => EConst(s.toDouble)
  }

  def unaryMinus:Parser[Expr] = "-" ~> term ^^ { EUnaryMinus(_) }

  def apply(s:String):Expr = {
    parse(s) match {
      case Success(tree, _) => tree
      case e: NoSuccess => throw new IllegalArgumentException("Bad syntax: " + s)
    }
  }

  // Simplify testing.
  def test(string:String) = {
    println("parsing " + string)
    parse(string) match {
      case Success(tree, _) =>
        println("Tree: "+tree)
        val v = tree.eval()
        println("Eval: "+v)
      case e:NoSuccess => Console.err.println(e)
    }
  }

  // A main method for testing.
  def main(args: Array[String]) = test(args(0))
}

object Main {
  def main(args: Array[String]) = 
    ExprParser.test("1")
    ExprParser.test("1 + 2")
    ExprParser.test("1 + 2 + 3")
    ExprParser.test("2 - 3")
    ExprParser.test("2 - 3 - 4")
    ExprParser.test("2 + 3 - 4")
    ExprParser.test("2 * 3 + 4")
    ExprParser.test("4 + 2 * 3")
    ExprParser.test("(4 + 2) * 3")
    ExprParser.test("(1 + 2)")
    ExprParser.test("(2 * 3)")
    ExprParser.test("1 + (2 * 3)")
    ExprParser.test("-1 + 3")
    ExprParser.test("-1 - 2")
    ExprParser.test("20.3")
    ExprParser.test("20.3 - 0.3 + -20")
}

// Useful when running as a script.
Main.main(argv)
