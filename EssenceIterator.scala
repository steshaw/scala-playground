//
// See http://dl.dropbox.com/u/5930402/EssenceIteratorPattern.odp
//     https://groups.google.com/d/msg/scala-melb/5VbJmfNK_h0/Cysnk1hUjoUJ
//

object EssenceIterator extends App {

  trait Monoid[T] {
    def zero: T
    def append(a1: T, a2: T): T
  }

  def accumulate[A: Monoid](list: List[A]): A = {
    val acc = implicitly[Monoid[A]]
    var total = acc.zero
    for (a <- list) {
      total = acc.append(total, a)
    }
    total
  }

  val intPlusMonoid = new Monoid[Int] {
    override def zero = 0
    override def append(a: Int, b: Int) = a + b
  }
  val intTimesMonoid = new Monoid[Int] {
    override def zero = 1
    override def append(a: Int, b: Int) = a * b
  }
  implicit val stringMonoid = new Monoid[String] {
    def zero = ""
    def append(a: String, b: String) = a + b
  }
  implicit def listMonoid[A] = new Monoid[List[A]] {
    def zero = List()
    def append(a: List[A], b: List[A]) = a ++ b
  }

  def sum(list: List[Int]): Int = accumulate(list)(intPlusMonoid)
  def product(list: List[Int]): Int = accumulate(list)(intTimesMonoid)
  def appendAll(list: List[String]) = accumulate(list)
  def flatten[A](list: List[List[A]]) = accumulate(list)

  val booleanAndMonoid = new Monoid[Boolean] {
    def zero = true
    def append(a: Boolean, b: Boolean) = a && b
  }
  val booleanOrMonoid = new Monoid[Boolean] {
    def zero = false
    def append(a: Boolean, b: Boolean) = a || b
  }
  def all(list: List[Boolean]) = accumulate(list)(booleanAndMonoid)
  def any(list: List[Boolean]) = accumulate(list)(booleanOrMonoid)

  // traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

  //
  // Validation with Applicative Functors
  // 

  case class Monster(name: String, health: Int)

  def createMonster(name: String, health: Int) = Monster(name, health)

  case class ValidationFailedException(errors: Seq[String]) extends Exception

  def createValidMonster(name: String, health: Int): Monster = {
    import scala.collection.mutable.ListBuffer
    val errors = ListBuffer.empty[String]
    var monster: Monster = null
    if (name.length > 0) {
      if (health > 0 && health < 100) {
        monster = createMonster(name, health)
      }
      else {
        errors += "Health must be > 0 and < 100"
      }
    }
    else {
      errors += "Name can't be empty"
    }
    if (errors.nonEmpty)
      throw new ValidationFailedException(errors)
    monster
  }

  // pure :: a -> f a
  // applic :: f (a -> b) -> f a -> f b

  trait Applicative[F[_]] {
    def pure[A](a: A): F[A]
    def applic[A, B](fab: F[A => B], fa: F[A]): F[B]
  }

  sealed trait Validation[+A]
  case class Success[+A](value: A) extends Validation[A]
  case class Failure(messages: List[String]) extends Validation[Nothing]

  def validationApplicative[A] = new Applicative[Validation] {
    def pure[A](a: A) = Success(a)
    def applic[A, B](fab: Validation[A => B], fa: Validation[A]) = (fab, fa) match {
      case (Success(f), Success(a)) => Success(f(a))
      case (Success(f), Failure(messages)) => Failure(messages)
      case (Failure(messages), Success(_)) => Failure(messages)
      case (Failure(messages1), Failure(messages2)) => Failure(messages1 ++ messages2)
    }
  }

  def validateName(name: String): Validation[String] =
    if (name.length > 0) Success(name) else Failure(List("Name can't be empty"))

  def validateHealth(health: Int): Validation[Int] =
    if (health > 0 && health < 100) Success(health) else Failure(List("Health must be > 0 and < 100"))

  def createValidMonster2(name: String, health: Int): Validation[Monster] = {
    val valName = validateName(name)
    val valHealth = validateHealth(health)
    val valCreateMonster = validationApplicative.pure((createMonster(_:String, _:Int)).curried)
    val b = validationApplicative.applic(valCreateMonster, valName)
    validationApplicative.applic(b, valHealth)
  }

  def explain(vm: Validation[Monster]) {
    vm match {
      case Success(monster) => println("Monster: " + monster)
      case Failure(errors) => println("Oops: "); errors foreach println
    }
  }

  explain(createValidMonster2("", 0))
  explain(createValidMonster2("Orc", 0))
  explain(createValidMonster2("Orc", 99))
}
