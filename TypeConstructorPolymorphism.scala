//
// From http://www.scala-lang.org/node/43
//
// Type constructor polymorphism was introducted in Scala 2.5 (02-May-2007).
//

trait Iterable[+t] {
  type MyType[+t] <: Iterable[t] // MyType is a type constructor

  def filter(p: t => Boolean): MyType[t] /* = ... */
  def map[s](f: t => s): MyType[s] /* = ... */
}

abstract class List[+t] extends Iterable[t] {
  type MyType[+t] = List[t]
}
