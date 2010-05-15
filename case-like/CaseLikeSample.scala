object CaseLikeSample extends Application {
  class Person(name: String, age: Int) extends Object with CaseLike {
    val parts = Seq(name, age)
  }
  val dave1 = new Person("Dave", 25)
  val dave2 = new Person("Dave", 25)
  val fred1 = new Person("Fred", 45)

  println(dave1)
  println(dave2)
  println(dave1 == dave2)
  println(fred1)
  println(fred1 == dave2)
  println(dave1 == fred1)
}
