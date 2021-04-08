//
// Adapted from https://groups.google.com/d/msg/scala-debate/VA4-qUxiN0I/9uGkbtgYToUJ
//
// Tony Morris motivates replacing dependency injection with
//
//  1. "threading" read only value through your computation. i.e reader monad
//  2. "threading" a modifiable value through your computation. i.e. state monad
//
// It seems to me that DI is mostly used for (1). i.e. your configuration is static and read-only (at least once
// the program is up and running - like slurped it's Spring beans config file etc). However, some of the 
// injected "services" will be mutable so perhaps in a FP sense, that is more like (2).
//
// I still wonder if there's some other utility gained from DI frameworks that are not supplied by the reader
// and or state monads. Something to think about is the AOP that is often associated with injected services.
// For instance, @Transaction method etc. There are, though, other means to more-or-less implement those using FP.
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

case class Context(val appName: String, val hostName: String, val port: Int)

{
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

  {
    // Here I use a function that accesses the context.
    def port(cx: Context) = cx.port

    // Still have to 'lift' it. Just wrap the fn in this case.
    def lift[A](f: Context => A) = ComputedWithContext(f)

    val port_ = lift(port)

    val result = for {
      a <- port_
      b <- e2_(a)
      c <- e3_(a, b)
      d <- e2_(c)
    } yield d
    println(result.cx(Context(appName = "di", hostName="localhost", port = 5000)))
  }
}

{
  case class WriteWithContext[A](cx: Context => (A, Context)) {
    def map[B](f: A => B): WriteWithContext[B] = WriteWithContext(c => {val (a, cc) = cx(c); (f(a), cc)})
    def flatMap[B](f: A => WriteWithContext[B]): WriteWithContext[B] = WriteWithContext(c => {
      val (a, cc) = cx(c)
      f(a) cx cc
    })
  }

  def lift0[A, B](a: A) = WriteWithContext((cx) => (a, cx))
  def lift1[A, B](f: A => B) = (a: A) => WriteWithContext((cx) => (f(a), cx))
  def lift2[A, B, C](f: (A, B) => C) = (a: A, b: B) => WriteWithContext((cx) => (f(a, b), cx))

  val e1_ = lift0(e1)
  val e2_ = lift1(e2)
  val e3_ = lift2(e3)

  {
    val result = for {
      a <- e1_
      b <- e2_(a)
      c <- e3_(a, b)
      d <- e2_(c)
    } yield d
    println(result.cx(Context(appName = "di", hostName="localhost", port = 5000)))
  }

  {
    // Here I use a function that accesses the context.
    def port(cx: Context) = cx.port

    // Still have to 'lift' it.
    def lift[A](f: Context => A) = WriteWithContext(cx => (f(cx), cx))

    val port_ = lift(port)

    val result = for {
      a <- port_
      b <- e2_(a)
      c <- e3_(a, b)
      d <- e2_(c)
    } yield d
    println(result.cx(Context(appName = "di", hostName="localhost", port = 5000)))

    // In this case, since hostName isn't used in the computation, it doesn't matter where it is "trashed".
    def trashHostName(cx: Context) = cx.copy(hostName = "oops")
    val trashHostName_ = WriteWithContext(cx => (Unit, trashHostName(cx)))

    {
      val result = for {
        _ <- trashHostName_
        a <- port_
        b <- e2_(a)
        c <- e3_(a, b)
        d <- e2_(c)
      } yield d
      println(result.cx(Context(appName = "di", hostName="localhost", port = 5000)))
    }

    {
      val result = for {
        a <- port_
        _ <- trashHostName_
        b <- e2_(a)
        c <- e3_(a, b)
        d <- e2_(c)
      } yield d
      println(result.cx(Context(appName = "di", hostName="localhost", port = 5000)))
    }
    {
      val result = for {
        a <- port_
        b <- e2_(a)
        _ <- trashHostName_
        c <- e3_(a, b)
        d <- e2_(c)
      } yield d
      println(result.cx(Context(appName = "di", hostName="localhost", port = 5000)))
    }
    {
      val result = for {
        a <- port_
        b <- e2_(a)
        c <- e3_(a, b)
        _ <- trashHostName_
        d <- e2_(c)
      } yield d
      println(result.cx(Context(appName = "di", hostName="localhost", port = 5000)))
    }
    {
      val result = for {
        a <- port_
        b <- e2_(a)
        c <- e3_(a, b)
        d <- e2_(c)
        _ <- trashHostName_
      } yield d
      println(result.cx(Context(appName = "di", hostName="localhost", port = 5000)))
    }
  }
}
