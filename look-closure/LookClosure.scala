import scala.collection.mutable.ListBuffer

def executeDelayedActions(delayedActions: Seq[() => Unit]) {
  for (k <- 0 until delayedActions.length) {
    delayedActions(k).apply()
  }
}

{
  // Fairly natural Scala version.
  val delayedActions: ListBuffer[() => Unit] = ListBuffer()

  for (i <- 4 to 6) {
    delayedActions += (() => println(i))
  }

  println("all same = " + delayedActions.forall(_ == delayedActions(0)))

  executeDelayedActions(delayedActions) // 4 5 6
}
{
  // Imperative while loop Scala version.
  val delayedActions: ListBuffer[() => Unit] = ListBuffer()

  var i = 4
  while (i < 7) {
    delayedActions += (() => println(i))
    i += 1
  }

  println("all same = " + delayedActions.forall(_ == delayedActions(0)))

  executeDelayedActions(delayedActions) // 7 7 7
}
{
  // Imperative Scala version with fix up.
  val delayedActions: ListBuffer[() => Unit] = ListBuffer()

  var i = 4
  while (i < 7) {
    var j = i
    delayedActions += (() => println(j))
    i += 1
  }

  println("all same = " + delayedActions.forall(_ == delayedActions(0)))

  executeDelayedActions(delayedActions) // 7 7 7
}
