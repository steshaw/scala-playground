// See DailyScala blog
object MultipleBounds extends Application {
  def process[R <: Function1[Int, String] with Iterable[String]](resource: R) = {
    println("using function: " + resource(0))
    println("using iterable: " + resource.iterator.next)
  }

  //process(Array(1,2,3).map(_.toString))
  process(List(1,2,3).map(_.toString))
}
