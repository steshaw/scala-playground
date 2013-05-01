/* 
 * Adapted from Erik Meijer keynote on "Fundamentalist Functional Programming"
 * from the slide on Variable Instantiation.
 * Scala acts similarly to C# with respect to variable capture.
 */

import scala.collection.mutable.ListBuffer

def printEm(actions : Iterable[() => Int]) {
  var s = ""
  for (action <- actions) {
    s += action()
  }
  println(s)
}

{
  val delayedActions = new ListBuffer[() => Int]

  var i = 4
  while (i < 7) {
    delayedActions += (() => i)
    i += 1
  }
  printEm(delayedActions)
}

{
  val delayedActions = new ListBuffer[() => Int]

  var i = 4
  while (i < 7) {
    val j = i;
    delayedActions += (() => j)
    i += 1
  }
  printEm(delayedActions)
}

{
  val delayedActions = new ListBuffer[() => Int]

  // More idiomatic Scala captures as expected.
  for (i <- (4 to 6)) {
    delayedActions += (() => i)
  }
  printEm(delayedActions)
}
