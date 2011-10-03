import scala.collection.mutable.ListBuffer

val delayedActions: ListBuffer[() => Unit] = ListBuffer()

for (i <- 4 to 6) {
  delayedActions += (() => println(i))
}

println("all same = " + delayedActions.forall(_ == delayedActions(0)))

for (k <- 0 until delayedActions.length) {
  delayedActions(k).apply()
}
