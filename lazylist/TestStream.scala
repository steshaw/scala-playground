//
// Matt Might's TestLazyList class converted to use Scala's built in Stream type. Using Scala 2.8 we can
// use the built in "syntax" for creating streams with the #:: cons operator.
//

object TestStream {

  def main (args : Array[String]) {

    val n2 = Stream.from(1) 
    println(n2.head)
    println(n2.tail.head)
    println(n2.tail.tail.head)
    // Prints:
    // 1
    // 2 
    // 3

    import Stream.{empty => snil}

    val safeLazy = 3 #:: {println("4 got eval'd"); 4} #:: snil
    // Prints nothing.

    val strictLazy = 5 +: { println("5 got eval'd") ;  6 }  +: snil
    // Prints:
    // 5 got eval'd

    val nukeMe = 1 #:: 2 #:: {(error("Boom!")) : Int} #:: snil

    // Pattern match:
    nukeMe match {
      case hd #:: _ => println("hd: " +hd)
      case _ => println("no match")
    }
    // Prints:
    // hd: 1

    println(nukeMe.head)
    // Prints:
    // 1

    println(nukeMe.tail.head)
    // Prints:
    // 2

    try {
      println (nukeMe.tail.tail)
    } catch {
      case e:Exception => println("nukeMe.rest.rest threw an Exception - " + e.getMessage)
    }
  }
}
