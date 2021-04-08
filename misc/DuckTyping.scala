object DuckTyping {

  class Duck {
    def quack() = println("Quaaaack!")
    def feathers() = println("The duck has white and grey feathers!")
  }

  class Person {
    def quack() = println("The person imitates a duck.")
    def feathers() = println("The person takes a feather from the ground and shows it.")
  }

  def inTheForest(duck: {def quack(); def feathers()}) {
    duck.quack()
    duck.feathers()
  }

  def game() {
    val duck = new Duck
    val person = new Person
    inTheForest(duck)
    inTheForest(person)
  }

  def main(args: Array[String]) = game()
}
