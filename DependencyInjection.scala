//
// Adapted from https://groups.google.com/d/msg/scala-debate/VA4-qUxiN0I/9uGkbtgYToUJ
//

def e1 = 12
def e2(n: Int) = n - 10
def e3(n: Int, m: Int) = n * m

//
// Original 'program'.
//
{
  var a = e1
  var b = e2(a)
  var c = e3(a, b)
  var d = e2(c)
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
  import Id._
  val Id(result) = for {
    a <- e1
    b <- e2(a)
    c <- e3(a, b)
    d <- e2(c)
  } yield d
  println(result)
}

//
// Original 'program' flatMapped.
// Trying flatMap to see if I can avoid the explicit Id calls (but no).
//
{
  import Id._
  var Id(d) = e1 flatMap(a =>
    e2(a) flatMap(b =>
    e3(a, b) flatMap(c =>
    e2(c))))
  println(d)
}

{
  case class Context(val appName: String, val hostName: String, val port: Int)

  case class ComputedWithContext[A](val cx: Context => A) {
    def map[B](f: A => B): ComputedWithContext[B] = ComputedWithContext(f compose cx)
    def flatMap[B](f: A => ComputedWithContext[B]): ComputedWithContext[B] = ComputedWithContext(c => f(cx(c)).cx(c))
  }

  def lift0[A, B](a: A) = ComputedWithContext((cx) => a)
  def lift1[A, B](f: A => B) = (a: A) => ComputedWithContext((cx) => f(a))
  def lift2[A, B, C](f: (A, B) => C) = (a: A, b: B) => ComputedWithContext((cx) => f(a, b))

  val e1_ = lift0(e1)
  val e2_ = lift1(e2)
  val e3_ = lift2(e3)

  val result = for {
    a <- e1_
    b <- e2_(a)
    c <- e3_(a, b)
    d <- e2_(c)
  } yield d
  println(result.cx(Context(appName = "di", hostName="localhost", port = 5000)))
}
