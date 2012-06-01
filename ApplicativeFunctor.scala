//
// Inspired by http://typeclassopedia.bitbucket.org/#slide-47
//

def parse(s : String) : Option[Int] =
  try {
    Some(s.toInt)
  } catch {
    case e : java.lang.NumberFormatException => None
  }

println(parse("12"))
println(parse("hi"))

val a0 = parse("3").map(x => (y: Int) => (x, y)).map(f => f(1))
println(a0)

trait Functor[F[_]] {
  def map[A, B](fa : F[A])(f : A => B) : F[B]
}

implicit val OptionFunctor = new Functor[Option] {
  def map[A, B](fa : Option[A])(f : A => B) : Option[B] = fa match {
    case Some(a) => Some(f(a))
    case None    => None
  }
}

trait Applicative[F[_]] extends Functor[F] {
  def <*>[A, B](fa : F[A])(f : F[A => B]) : F[B]
}

implicit val OptionApplicative = new Applicative[Option] {
  def map[A, B](fa : Option[A])(f : A => B) : Option[B] = fa match {
    case Some(a) => Some(f(a))
    case None    => None
  }
  def <*>[A, B](fa : Option[A])(f : Option[A => B]) : Option[B] = (fa, f) match {
    case (Some(a), Some(f)) => Some(f(a))
    case _                  => None
  }
}

def applic[F[_] : Applicative, A, B](fa: F[A])(f : F[A => B]) : F[B] = implicitly[Applicative[F]].<*>(fa)(f)

val a1 = applic(parse("1"))(parse("3").map(x => (y: Int) => (x, y)))
println(a1)
