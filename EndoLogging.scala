
trait Monoid[T] {
  def empty: T
  def append(t1: T, t2: T): T
}

case class Endo[A](a: A => A)

object Endo {
  def log(message: String): Endo[List[String]] = Endo(as => message::as)

  def collapse[A](a: List[Endo[A]]) = a.foldRight(Endo(id[A])) {
    case (Endo(a), Endo(b)) => Endo(a compose b)
  }

  def id[A](a: A) = a

  def frobnicate[T](a: Endo[T], start: T): T = {
    a match {
      case Endo(f) => f(start)
    }
  }

  implicit def endoAccumulate[T]: Monoid[Endo[T]] = new Monoid[Endo[T]] {
    def empty = Endo(id[T])

    def append(t1: Endo[T], t2: Endo[T]) = Endo(t1.a compose t2.a)
  }
}

object EndoDemo {
  def main(args: Array[String]) {
    import Endo._

    val logging = List(log("hi"), log("there"))

    val endoResult: Endo[List[String]] = collapse(logging)

    val result: List[String] = endoResult match {
      case Endo(f) => f(List.empty)
    }

    result foreach println
  }
}

//
// See Tony Morris' "Logging Without Side Effects"
// http://dl.dropbox.com/u/7810909/writer-monad/chunk-html/index.html
//

object WriterDemo {
  object WriterLog {
    type LOG = List[String]
  }

  import WriterLog._

  case class Writer[A](log: LOG, a: A) {
    def map[B](f: A => B): Writer[B] = Writer(log, f(a))

    def flatMap[B](f: A => Writer[B]): Writer[B] = {
      val Writer(log2, b) = f(a)
      Writer(log ::: log2 /* accumulate */, b)
    }
  }

  object Writer {
    implicit def LogUtilities[A](a: A) = new {
      def nolog = Writer(Nil /*empty*/, a)
      def withLog(log: String) = Writer(List(log), a)
      def withValueLog(log: A => String) = withLog(log(a))
    }
  }

  import Writer._

  def main(args: Array[String]) = {
    val k = args(0).toInt

    val result = for {
      a <- k withValueLog ("starting with " + _)
      b <- (a + 7) withLog "adding 7"
      c <- (b * 3).nolog
      d <- c.toString.reverse.toInt withValueLog ("switcheroo with " + _)
      e <- (d % 2 == 0) withLog "is even?"
    } yield e

    println("Result: " + result.a)
    println("LOG")
    println("===")
    result.log foreach println
  }
}

/**
 * Mix my Endo "implementation" above with Tony's WriteDemo.
 */
object EndoWriterDemo {

  object WriterLog {
    type LOG = Endo[List[String]]
    type Logging_UNUSED[LOG, A] = (LOG, A)
  }
  import WriterLog._

  case class Writer[A](log: LOG, a: A) {
    def map[B](f: A => B): Writer[B] = Writer(log, f(a))

    def flatMap[B](f: A => Writer[B])(implicit acc: Monoid[LOG]): Writer[B] = {
      val Writer(log2, b) = f(a)
      Writer(acc.append(log, log2), b)
    }
  }

  import Endo._

  object Writer {
    implicit def LogUtilities[A](a: A)(implicit acc: Monoid[LOG]) = new {
      def nolog = Writer(acc.empty, a)
      def withLog(message: String) = Writer(acc.append(log(message), acc.empty), a)
      def withValueLog(mkMessage: A => String) = withLog(mkMessage(a))
    }
  }

  import Writer._

  def main(args: Array[String]) = {
    val k = args(0).toInt

    val result = for {
      a <- k withValueLog ("starting with " + _)
      b <- (a + 7) withLog "adding 7"
      c <- (b * 3).nolog
      d <- c.toString.reverse.toInt withValueLog ("switcheroo with " + _)
      e <- (d % 2 == 0) withLog "is even?"
    } yield e

    println("Result: " + result.a)
    println("LOG")
    println("===")
    frobnicate(result.log, List.empty) foreach println
  }
}

object Demos {
  def main(args: Array[String]) {
    EndoDemo.main(Array())
    println()
    WriterDemo.main(Array("41"))
    println()
    EndoWriterDemo.main(Array("41"))
  }
}

//Demos.main(Array())
