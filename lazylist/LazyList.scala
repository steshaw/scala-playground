// See http://matt.might.net/articles/implementation-of-lazy-list-streams-in-scala/

/**
 A LazyList is strict on its head, but lazy on its tail.
 */
abstract class LazyList[+A] {
  def first : A ;
  def rest : LazyList[A] ;

  /**
   *>>: evaluates right-to-left, and it is *strict* on its second argument.

   You probably don't want to use this.  It's provided for completeness.

   *>> with manual parenthetical grouping constructs truly lazy lists.
   */
  def *>>: [U >: A] (head : U) : LazyList[U] = new LazyCons[U](head,this)
}

/**
 A LazyCons is strict on the head, but lazy on the tail.
 */
class LazyCons[A] (val first : A, _rest : => LazyList[A]) extends LazyList[A] {
  lazy val rest = _rest
}

/**
 A LazyNil terminates a lazy list.
 */
case object LazyNil extends LazyList[Nothing] {
  def first : Nothing = throw new Exception("Can't take first of LazyNil")
  def rest : LazyList[Nothing] = throw new Exception("Can't take rest of LazyNil")
}

/**
 For pattern-mathing lazy lists.
 */
object *>>: {
  def unapply[A] (llist : LazyList[A]) : Option[(A,LazyList[A])] = {
    if (llist == LazyNil) 
      None
    else
      Some(llist.first,llist.rest)
  }
}


/**
 LazyConsable objects support lazy list creation with the operator *>>.
 */
trait LazyConsable[A] {
  def *>> (rest : => LazyList[A]) : LazyList[A] ;
}


/**
 LazyListImplicit contains implicit methods for adding *>> to every object in the language.
 */
object LazyListImplicit {
  implicit def lazyConsable[A] (first : A) : LazyConsable[A] = new LazyConsable[A] {
    def *>> (rest : => LazyList[A]) = new LazyCons(first,rest)
  }
}


/**
 Tests LazyList functionality.
 */
object TestLazyList {

  import LazyListImplicit._ 

  def main (args : Array[String]) {

    def intsFrom(n : Int) : LazyList[Int] = n *>> intsFrom(n+1)

    val n2 = intsFrom(1) 
    println(n2.first) ;
    println(n2.rest.first) ;
    println(n2.rest.rest.first) ;
    // Prints:
    // 1
    // 2 
    // 3
    
    val safeLazy : LazyList[Int] = 3 *>> ({ println("4 got eval'd") ;  4 }  *>> LazyNil)
    // Prints nothing.

    val strictLazy : LazyList[Int] = 5 *>>: { println("5 got eval'd") ;  6 }  *>>: LazyNil
    // Prints:
    // 5 got eval'd
    
    val nukeMe : LazyList[Int] = 1  *>> (2  *>> (throw new Exception("Boom!")))

    // Pattern match:
    nukeMe match {
      case hd *>>: _ => println("hd: " +hd)
      case _ => println("no match")
    }
    // Prints:
    // hd: 1
    

    println (nukeMe.first)
    // Prints:
    // 1

    println (nukeMe.rest.first)
    // Prints:
    // 2
    
    try {
      println (nukeMe.rest.rest)
    } catch {
      case _ => println ("nukeMe.rest.rest threw an Exception")
    }
  }
}

