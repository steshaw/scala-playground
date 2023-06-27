def option1 =
  enum Option[+T]:
    case Some(x: T)
    case None

  import Option.*

  val n42 = Some(42)
  val none = None
  {
    val x: Option[Int] = Some(42)
    println(x)
  }
  {
    val x: Option[Int] = None
    println(x)
  }

def option2 =
  trait Option[+T]
  case class Some[T](x: T) extends Option[T]
  case object None extends Option[Nothing]

  import Option.*

  val n42 = Some(42)
  val none = None
  {
    val x: Option[Int] = Some(42)
    println(x)
  }
  {
    val x: Option[Int] = None
    println(x)
  }

def option3 =
  trait Option[T]
  case class Some[T](x: T) extends Option[T]
  case class None[T]() extends Option[T]

  import Option.*

  val n42 = Some(42)
  val none = None()
  {
    val x: Option[Int] = Some(42)
    println(x)
  }
  {
    val x: Option[Int] = None()
    println(x)
  }

object Option:
  def main(args: Array[String]) =
    option1
    option2
    option3
