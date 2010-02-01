
object ContagiousLazinessOfRange {
  case class User(name: String, age: Int)

  // In Scala 2.7, this just prints "hi", whereas in 2.8 it prints the users and then "hi".
  def main(args: Array[String]) = {
    val users = for (i <- 1 to 20) yield {
      val user = User("Steven" + i, 30+i)
      println(user)
      user
    }
    println("hi")
  }
}
