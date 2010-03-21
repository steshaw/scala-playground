package scalax.logic
 
object MiniKanren extends util.logging.ConsoleLogger {
  
  type Subs = List[Subst];
  type Goal = Subst => Subs;
  type Goals = Seq[Goal];
  
  final val EmptySubs = Nil;
  final val Failure = Subst(false, EmptySubs);
  final val Success = Subst(true, EmptySubs);
 
  final val logging = false;
  
  final def logx[K](k: K): K = { if (logging) log(k.toString); k }
  
  final case class Binding(v: Var, x: Term) {
    override def toString(): String = v.name.toString() + '=' + x.x;
  }
  
  def runs(v: Var, goals: Goal*): Terms = 
    Terms(All(goals:_*)(Success).map(s => reify(s.walk_s(v))));
  
  def fresh(prefix: String): Var = synchronized { nextFresh = nextFresh + 1; Var(new Symbol(prefix + '.' + nextFresh)) }
  final def fresh: Var = fresh("_");
  
  private def reify(v: Term): Term = Success.reify_s(v).walk_s(v);
    
  def bind(ss: Subs, g: Goal): Subs = ss.flatMap(s => g(s));
  
  ////////////////////////////////////////////////
  // Utilities
  ////////////////////////////////////////////////
  private var nextFresh = 0;
  
  implicit def boolToSubst(b: Boolean) = if (b) Success else Failure;
  implicit def symToVar(name: Symbol): Var = Var(name);
  implicit def makeValue(x: Any): Value = x match {
      case v: Value => v
      case _ => Value(x)
  }
  implicit def makeGoals(g: Goal): Goals = Array(g);
  
  def subs(s: Subst) = s :: EmptySubs;
  def stream[K](seq: Seq[K]): List[K] = seq.toList;
  def terms(t: Term*): Terms = Terms(t);
  
}
 
import MiniKanren._;
 
sealed trait Term {
  def *==(x: Any) = x match {
    case t: Term => Eq(this, t)
    case s: Symbol => Eq(this, Var(s))
    case _ => Eq(this, Value(x))
  }
}
 
final case class Value(x: Any) extends Term;
  
final case class Var(name: Symbol) extends Term {
  override def toString(): String = name.toString();
}
  
final case class Subst(success: boolean, binds: Seq[Binding]) extends Term {
    
    def reifiedName = "_." + binds.length;
    override def toString: String = binds.foldLeft("[ ")((s, b) => s + b + ' ') + "]";
    
    def walk(v: Term): Term = v match {
      case Var(name) => 
        if (logging) log("Walk for " + name)
        binds.elements.find(b => b.v == v) match {
          case Some(Binding(vr, value)) =>
            walk(value)
          case _ => v
      }
      case _ => v
    }
      
    def walk_s(v: Term): Term = walk(v) match {
      case Terms(lst) => Terms(lst.toList.map(t => walk_s(t)))
      case t: Term => t
    }
    
    def reify_s(v: Term): Subst = walk(v) match {
      case m: Var => Subst(true, Binding(m, Value(reifiedName)) :: binds.toList)
      case Terms(m) => m.foldLeft(this)((ss,t) => ss.reify_s(t))
      case _ => this
    }
    def unify(v: Term, w: Term): Subst = {
      if (logging) log("Unify " + v + ' ' + w + ' ' + toString)
      this match {
        case Failure => this
        case _ => Pair(walk(v), walk(w)) match {
          case Pair(m,n) if (m eq n) => this 
          case Pair(m : Var, n) => Subst(true, Binding(m,n) :: binds.toList);
          case Pair(m, n : Var) => Subst(true, Binding(n,m) :: binds.toList);
          case Pair(Terms(m), Terms(n)) =>
            m.toList.zip(n.toList).foldLeft(this)((ss, z) => ss.unify(z._1, z._2))
          case Pair(m,n) if (m == n) => this
          case _ => Failure
        }
      }
    }
}
  
final case class Terms(lst: Seq[Term]) extends Term {
  def map[b](f: Term => b) = lst.toList.map(f);
  def flatMap[b](f: Term => List[b]) = lst.toList.flatMap(f);
  def filter(p: Term => Boolean) = lst.toList.filter(p);
  def foreach(f: Term => Unit) = lst.foreach(f);
  override def toString: String = lst.foldLeft("Terms( ")((s, b) => s + b + ' ') + ")";
}
  
final case class All(goals: Goal*) extends Goal {
  def apply(s: Subst) = goals.foldLeft(subs(s))((ss,g) => bind(ss, g));
}
    
final case class Eq(a: Term, b: Term) extends Goal {
  def apply(s: Subst) = s.unify(a,b) match {
    case k @ Subst(true, _) => subs(k)
    case _ => EmptySubs
  }
}
  
final case class Conde(gl: Goals*) extends Goal {
  def apply(s: Subst) = stream(gl).map(gs => All(gs:_*)).flatMap(g => g(s));
}
