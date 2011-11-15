//
// Adapted from http://blog.tmorris.net/type-safe-scala-sequence-comprehensions/
//
// Refutes the statements in p85 of "Scala By Example":
//
// "It would be attractive to enforce these types statically in the Scala compiler, for instance by requiring that 
// any type supporting for-comprehensions implements a standard trait with these methods 1. The problem is that such 
// a standard trait would have to abstract over the identity of the class C, for instance by taking C as a type 
// parameter. Note that this parameter would be a type constructor, which gets applied to several different types in 
// the signatures of methods map and flatMap. Unfortunately, the Scala type system is too weak to express this 
// construct, since it can handle only type parameters which are fully applied types."
//
// Of course, this is well-known now, however the documentation hasn't been updated.
//
// Scala has supported "higher kinds" since version 2.5 (release in 21-May-2007).
// See: http://www.scala-lang.org/node/155#2.5.0-final "Added support for type constructor polymorphism."
// and: http://www.scala-lang.org/node/43#2.5.0 "Type constructor polymorphism"
//

trait Functor[f[_], a] {
  def map[b](f: a => b)(ft: => f[a]): f[b]
}

trait Monad[m[_], a] {
  def flatMap[b](ma: => m[a])(f: a => m[b]): m[b]
}

trait Filter[c[_], a] {
  def filter(f: a => Boolean)(c: => c[a]): c[a]
}

object Comprehension {
  type Comprehension[c[_], a] = Functor[c, a] with Monad[c, a] with Filter[c, a]

  def foreach[c[_], a](c: c[a], e: a => Unit, p: a => Boolean)(implicit ca: Comprehension[c, a]): Unit = 
    ca.map(a => if(p(a)) e(a))(c)

  def foreach[c[_], a](c: c[a], e: a => Unit)(implicit ca: Comprehension[c, a]): Unit = 
    foreach[c, a](c, e, (a: a) => true)

  def foreach[c[_], a, b](c: c[a], f: a => b, p: a => Boolean)(implicit ca: Comprehension[c, a]): c[b] = 
    ca.map(a => f(a))(ca.filter(p)(c))

  def foreach[c[_], a, b](c: c[a], f: a => b)(implicit ca: Comprehension[c, a]): c[b] = 
    ca.map(a => f(a))(c)
}
