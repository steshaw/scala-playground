//
// Adapted from https://groups.google.com/d/msg/scala-debate/VA4-qUxiN0I/9uGkbtgYToUJ
//

sealed abstract class Case
case object AllUpperCase extends Case
case object NotAllUpperCase extends Case

def e1 = "HELLO"
def e2(s: String) = s.toUpperCase
def e3(s1: String, s2: String) = s1 == s2
def e4(b: Boolean) = if (b) AllUpperCase else NotAllUpperCase

//
// Original 'program'.
//
{
  var a = e1
  var b = e2(a)
  var c = e3(a, b)
  var d = e4(c)
  println(d)
}

case class Id[A](i: A) {
  def map[B](f: A => B) = Id(f(i))
  def flatMap[B](f: A => Id[B]) = f(i)
}
object Id {
  implicit def IdIn[A](a: A) = Id(a)
  implicit def IdOut[A](a: Id[A]) = a.i
}

//
// Original 'program' wrapped in a for expression.
//
{
  val result = for {
    a <- Id(e1)
    b <- Id(e2(a))
    c <- Id(e3(a, b))
    d <- Id(e4(c))
  } yield d
  println(result)
}

//
// Original 'program' flatMapped.
// Trying flatMap to see if I can avoid the explicit Id calls (but no).
//
{
  var d = Id(e1) flatMap(a =>
    Id(e2(a)) flatMap(b =>
    Id(e3(a, b)) flatMap(c =>
    e4(c))))
  println(d)
}
