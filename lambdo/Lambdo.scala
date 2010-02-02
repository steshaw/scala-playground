/*

 Lambdo is a toy lambda-calculus-based language.

 This is a denotational interpreter.  Instead of passing a store (to
 remain purely functional), it piggybacks off Scala's store with
 side-effecting cells.

 Author: Matthew Might
 Site:   http://matt.might.net/

 */

import languages.sexp._ ;

object LambdoSyntax {
  import SExpSyntax._ ;

  // Helpers:
  private def safeMatch[A,B] (x : A) (f : (A) => B) : Option[B] =
    try { 
      Some(f(x)) 
    } catch {
      case (me : MatchError) => None
    }

  // Custom deconstructor pattern matchers for S-Expressions:
  object SLambda {
    // unapply() is the inverse of apply()
    def unapply(sx : SExp) : Option[(List[S], SExp)] = safeMatch (sx) {
      case L(S("lambda") :: L(sxvars) :: List(sxbody)) =>
        (sxvars.map(_.asInstanceOf[S]), sxbody)
    }
  }
  
  object SDefine {
    def unapply(sx : SExp) : Option[(SExp, SExp)] = safeMatch (sx) {
      case L(S("define") :: formula :: List(value)) => (formula, value)
    }
  }

  object SIf {
    def unapply(sx : SExp) : Option[(SExp,SExp,SExp)] = safeMatch (sx) {
      case L(S("if") :: cond :: cons :: alt :: List()) => (cond,cons,alt)
    }
  }

  object SLet1 {
    def unapply(sx : SExp) : Option[(S,SExp,SExp)] = safeMatch (sx) {
      case L(S("let") :: L(List(L(List(v, value)))) :: body :: List()) => (v.asInstanceOf[S], value, body)
    }
  }

  object SLetRec {
    def unapply(sx : SExp) : Option[(List[S], List[SExp], SExp)] = safeMatch (sx) {
      case L(S("letrec") :: L(clauses) :: body :: List()) => { 
        val namesXvalues : List[(S,SExp)] = clauses map ({case L(List(name,value)) => (name.asInstanceOf[S],value)})
        val (names,values) = List.unzip(namesXvalues)
        (names,values,body)
      }
    }
  }

  object SSet {
    def unapply(sx : SExp) : Option[(S, SExp, Option[SExp])] = safeMatch (sx) {
      case L(S("set!") :: v :: value :: List()) => (v.asInstanceOf[S], value, None)
      case L(S("set!") :: v :: value :: body :: List()) => (v.asInstanceOf[S], value, Some(body))
    }
  }

  object SBegin {
    def unapply(sx : SExp) : Option[List[SExp]] = safeMatch (sx) {
      case L(S("begin") :: cmds) => cmds
    }
  }

  /* Definitions */
  abstract class Def(val name : String) 

  case class VarDef (n : String, value : Exp) extends Def(n)
  case class FunDef (n : String, args : List[String], value : Exp) extends Def(n)
  case class ImpDef (value : Exp) extends Def("last")

  /* Expressions */
  abstract class Exp

  // Core:
  case class Lambda (params : List[String], body : Exp) extends Exp
  case class Var (name : String) extends Exp
  case class App (f : Exp, args : List[Exp]) extends Exp

  // Sugar:
  case class If (cond : Exp, ifTrue : Exp, ifFalse : Exp) extends Exp
  case class Let1(name : String, value : Exp, body : Exp) extends Exp
  case class LetRec(names : List[String], values : List[Lambda], body : Exp) extends Exp
  case class Set(name : String, value : Exp, body : Exp) extends Exp
  case class Seq(exps : List[Exp]) extends Exp

  // Literals:
  case class Void extends Exp
  case class BoolTrue extends Exp
  case class BoolFalse extends Exp
  case class IntNum (z : BigInt) extends Exp
  case class EmptyList() extends Exp
  case class PrimOp(p : String) extends Exp

  // parseDef: Converts an S-Expression into a Def.
  def parseDef (sx : SExp) : Def = {
    sx match {
      case SDefine(name : S, value) =>
        VarDef(name.symbol, parseExp(value))

      case SDefine(L((f : S) :: (args : List[S])), body) =>
        FunDef(f.symbol, args map (_.symbol), parseExp(body))
      
      case sx => ImpDef(parseExp(sx))
    }
  }

  // parseExp: Converts an S-Expression into an Exp.
  def parseExp (sx : SExp) : Exp = {
    sx match {
      case S(p @ ("+" | "-" | "/" | "*" | "=" | "<")) => PrimOp(p)
      case S("#f") => BoolFalse()
      case S("#t") => BoolTrue()
      case Z(z) => IntNum(z)
      case S(v) => Var(v)

      case SIf(cond, cons, alt) => 
        If(parseExp(cond), parseExp(cons), parseExp(alt))
      case SLambda(names, body) =>
        Lambda(names map (_.symbol), parseExp(body))
      case SLetRec(names, values, body) =>
        LetRec(names map (_.symbol),
               values map (((x : Exp) => x.asInstanceOf[Lambda]) compose parseExp),
               parseExp(body))
      case SLet1(name, value, body) =>
        Let1(name.symbol, parseExp(value), parseExp(body))
      case SSet(name, value, Some(body)) =>
        Set(name.symbol, parseExp(value), parseExp(body))
      case SSet(name, value, None) =>
        Set(name.symbol, parseExp(value), Void())
      case SBegin(exps) =>
        Seq(exps map parseExp)

      case L(f :: args) => App(parseExp(f), args map parseExp)
      case L(List()) => EmptyList()
    }
  }
}


object SemanticDomains {

  import LambdoSyntax._

  type Value = SExp // Just like in Scheme!
  type Env = scala.collection.immutable.Map[String, Cell]

  // Scala allows new cases to appear anywhere.
  case class Closure(lam : Lambda, env : Env) extends SExp
  case object True extends SExp
  case object False extends SExp
  case object Unit extends SExp
  case class Prim(p : String) extends SExp

  class Cell(var value : Value) 
}


class LambdoInterpreter {

  import SExpSyntax._
  import LambdoSyntax._
  import SemanticDomains._

  // cell: Allocate a new cell for a value.
  def cell (value : Value) : Cell = new Cell(value)

  // Environments:
  val emptyEnv : Env = scala.collection.immutable.TreeMap[String,Cell]()
  val globalEnv = scala.collection.mutable.HashMap[String,Value]()

  // lookup: Looks up a variable in an environment, but falls back on
  // the global environment.
  def lookup (e : Env) (v : String) : Value = (e get v) match {
    case Some(cell) => cell.value
    case None => globalEnv(v)
  }

  // set: Set a variable's value in an environment if it exists, and
  // set it in the global environment if not.
  def set (e : Env) (v : String) (value : Value) = (e get v) match {
    case Some(cell) => cell.value = value
    case None => globalEnv(v) = value
  }

  // process: Enters a definition into the system.
  def process (d : Def) : Value = {
    val res = d match {
      case VarDef(name, value) =>  eval(value,emptyEnv)
      case FunDef(name, args, body) => eval(Lambda(args,body),emptyEnv)
      case ImpDef(value) => eval(value,emptyEnv)
    }
    globalEnv(d.name) = res
    res
  }

  // eval: Evaluates an expression in the context of an environment.
  def eval (exp : Exp, env : Env) : Value = exp match {

    // Literals:
    case BoolTrue() => True
    case BoolFalse() => False
    case IntNum(z) => Z(z)
    case PrimOp(p) => Prim(p)
    case EmptyList() => L(List())
    case Void() => Unit

    // Core:
    case Var(name) => lookup (env) (name)

    case lam @ Lambda(args, body) => Closure(lam, env)

    case App(f, args) => {
      val proc = eval(f,env)

      val vals : List[Value] = args map (eval(_,env))

      (proc,vals) match {
        case (Closure(Lambda(vars,body), env2),_) =>
          eval(body, env2 ++ (vars zip (vals map (cell _))))

        case (Prim("+"),List(Z(a), Z(b))) => Z(a + b)
        case (Prim("-"),List(Z(a), Z(b))) => Z(a - b)
        case (Prim("*"),List(Z(a), Z(b))) => Z(a * b)
        case (Prim("/"),List(Z(a), Z(b))) => Z(a / b)
        case (Prim("<"),List(Z(a), Z(b))) => if (a < b) True else False
        case (Prim("="),List(Z(a), Z(b))) => if (a == b) True else False
      }
    }
 
    // Sugar:
    case If(cond,cons,alt) =>
      if (eval(cond,env) != False)
        { eval(cons,env) }
      else
        { eval(alt,env) }

    case LetRec(names, funs, body) => {
      val cells = funs map ((_ : Lambda) => cell(null))
      val namesXcells = names zip cells
      val env2 = env ++ namesXcells
      val vals = funs map (eval(_,env2))

      for ((c,value) <- cells zip vals) {
        c.value = value
      }

      eval(body,env2)
    }

    case Let1(name,value,body) => 
      eval(body, env(name) = cell(eval(value,env)))

    case Set(name,value,body) => {
      set (env) (name) (eval(value,env))
      eval(body,env)
    }
    
    case Seq(List()) => Unit
    case Seq(List(exp)) => eval(exp,env)
    case Seq(exp :: rest) => {
      eval(exp,env)
      eval(Seq(rest),env)
    }
  }
}

object Lambdo extends Application {
  val stdin : String = (scala.io.Source.fromInputStream(System.in)) mkString ""
  val sxs : List[SExp] = SParser.parse(stdin)
  val defs : List[LambdoSyntax.Def] = sxs map LambdoSyntax.parseDef  
  val interp = new LambdoInterpreter

  for (d <- defs) {
    println(interp.process(d))
    println()
  }
}
