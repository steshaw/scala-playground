//
// Adapted from https://gist.github.com/markhibberd/10784706
//

import scala.collection.immutable.TreeSet

object LosingAnInstance extends App {
  // scala.collection.immutable.TreeSet does what you suggest with closing over Ord
  // but it means I can write code like below. This can happen in far more subtle
  // and insiduous ways if local type classes are a common idiom in your code.
 
  def reversed =
    implicitly[Ordering[Int]].reverse
 
  def runContextOne = {
    implicit val IntOrdering: Ordering[Int] = reversed
    val start = TreeSet(1, 7, 3, 5)
    println("I am ordered in reverse: " + start)
    // => I am ordered in reverse: TreeSet(7, 5, 3, 1)
    val updated = ContextTwo.addOne(start)
    println("What do I look like now? " + updated)
    // => What do I look like now? TreeSet(2, 4, 6, 8)
    ()
  }
 
  object ContextTwo {
    def addOne(s: TreeSet[Int]) =
      s.map(_ + 1)
  }
 
  runContextOne
}
LosingAnInstance.main(Array())
