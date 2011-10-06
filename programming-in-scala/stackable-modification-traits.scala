
abstract class IntQueue {
  def get(): Int
  def put(a: Int)
}

class BasicIntQueue extends IntQueue {
  import scala.collection.mutable.ArrayBuffer
  /*private*/ val buf = new ArrayBuffer[Int]
  def get() = buf.remove(0)
  def put(a: Int) { buf += a }
}

trait Doubling extends IntQueue {
  abstract override def put(a: Int) { super.put(2 * a) }
}

{
  println("BasicIntQueue withDoubling")
  val queue = new BasicIntQueue with Doubling

  queue.put(10)

  val result = queue.get()
  println(result)
  println

  assert(queue.buf.isEmpty)
}

trait Incrementing extends IntQueue {
  abstract override def put(a: Int) { super.put(a + 1) }
}

trait Filtering extends IntQueue {
  abstract override def put(a: Int) { if (a >= 0) super.put(a) }
}

{
  println("BasicIntQueue Incrementing with Filtering")
  val queue = new BasicIntQueue with Incrementing with Filtering

  List(-1, 0, 1) foreach queue.put

  val result = queue.get()
  println(result)

  {
    val result = queue.get()
    println(result)
  }
  println

  assert(queue.buf.isEmpty)
}

{
  println("BasicIntQueue Incrementing with Doubling with Incrementing with Filtering")
  implicit val queue = new BasicIntQueue with Doubling with Incrementing with Filtering

  for (i <- -10 to 10) queue.put(i)

  def printGet(implicit queue: IntQueue) {
    val result = queue.get
    println(result)
  }

  for (i <- 0 to 10) printGet
  println

  assert(queue.buf.isEmpty)
}

// However, we cannot inherit Doubling twice :(.
//val queue = new BasicIntQueue with Doubling with Doubling with Filtering
