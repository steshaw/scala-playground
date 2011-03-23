
object UnusedDefinitions {
    type Logging[LOG, A] = (LOG, A)
}

trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def map[A, B](a: M[A], f: A => B): M[B]
  def flatMap[A, B](a: M[A], f: A => M[B]): M[B]
}

trait Monoid[T] {
  def empty: T
  def append(t1: T, t2: T): T
}

object ListX {
  implicit def instanceMonoid[T] : Monoid[List[T]] = new Monoid[List[T]] {
    def empty = List.empty
    def append(t1: List[T], t2: List[T]) = t1 ::: t2
  }

  implicit def instanceForeachPrintln[T]: ForeachPrintln[List[T]] = new ForeachPrintln[List[T]] {
    def foreachPrintln(as: List[T]) {
      as foreach println
    }
  }

  implicit def instanceStringToLog: StringToLog[List[String]] = new StringToLog[List[String]] {
    def stringToLog(s: String) = List(s)
  }

  implicit def instanceMonad: Monad[List] = new Monad[List] {
    def unit[A](a: A) = List(a)
    def map[A, B](as: List[A], f: A => B) = as.map(f)
    def flatMap[A, B](as: List[A], f: A => List[B]) = as.flatMap(f)
  }
}

trait ForeachPrintln[T] {
  def foreachPrintln(t: T): Unit
}

trait StringToLog[T] {
  def stringToLog(s: String): T
}

//import Monoid._

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

  implicit def instanceMonoid[T]: Monoid[Endo[T]] = new Monoid[Endo[T]] {
    def empty = Endo(id[T])
    def append(t1: Endo[T], t2: Endo[T]) = Endo(t1.a compose t2.a)
  }

  implicit def instanceForeachPrintln[T]: ForeachPrintln[Endo[List[T]]] = new ForeachPrintln[Endo[List[T]]] {
    def foreachPrintln(a: Endo[List[T]]) {
      val list = a match {
        case Endo(f) => f(List.empty)
      }
      list foreach println
    }
  }

  implicit def instanceStringToLog: StringToLog[Endo[List[String]]] = new StringToLog[Endo[List[String]]] {
    def stringToLog(s: String) = Endo.log(s)
  }

  implicit def instanceMonad: Monad[Endo] = new Monad[Endo] {
    def unit[A](a: A) = Endo(id)
    def map[A, B](as: Endo[A], f: A => B) = flatMap(as, unit[A])
    def flatMap[A, B](as: Endo[A], f: A => Endo[B]) = error("todo instanceMonad.flatMap")
  }
}

case class Consing[T](as: List[T])

object Consing {
  implicit def instanceMonoid[T]: Monoid[Consing[T]] = new Monoid[Consing[T]] {
    def empty = Consing(List.empty)
    def append(t1: Consing[T], t2: Consing[T]) = {
      println("append t1="+t1+" t2="+t2)
      if (t1.as.length == 0) t2
      else if (t1.as.length == 1) Consing(t1.as.head :: t2.as)
      else error("oops - was expecting list of length 1 as first argument -- length(as) = " + t1.as.length)
    }
  }

  implicit def instanceForeachPrintln[T]: ForeachPrintln[Consing[T]] = new ForeachPrintln[Consing[T]] {
    def foreachPrintln(as: Consing[T]) {
      as.as foreach println
    }
  }

  implicit def instanceStringToLog: StringToLog[Consing[String]] = new StringToLog[Consing[String]] {
    def stringToLog(s: String) = Consing(List(s))
  }

  implicit def instanceMonad: Monad[Consing] = new Monad[Consing] {
    def unit[A](a: A) = Consing(List(a))
    def map[A, B](as: Consing[A], f: A => B) = Consing(as.as.map(f))
    def flatMap[A, B](as: Consing[A], f: A => Consing[B]) = Consing(as.as.flatMap(a => f(a).as))
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
      Writer(log ::: log2, b)
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

  def frobnicate(a: List[String], ignore: List[String]) = a

  def go(n: Int) {
    val result = for {
      a <- n withValueLog ("starting with " + _)
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

  def main(args: Array[String]) = {
    val k = args(0).toInt
    go(k)
  }
}

/**
 * Mix my Endo "implementation" above with Tony's WriteDemo.
 */
object EndoWriterDemo {

  object WriterLog {
    type LOG = Endo[List[String]]
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
      def withLog(message: String) = Writer(log(message), a)
      def withValueLog(mkMessage: A => String) = withLog(mkMessage(a))
    }
  }

  import Writer._

  def go(n: Int) {
    val result = for {
      a <- n withValueLog ("starting with " + _)
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

  def main(args: Array[String]) = {
    val n = args(0).toInt
    go(n)
  }
}

/**
 * Generalise the WriterDemo to any Monoid.
 * Show examples with Endo[List[String]] and simply List[String].
 */
class MonoidWriterDemo[M[_], T](
    implicit val monoid: Monoid[M[T]],
    implicit val fe: ForeachPrintln[M[T]],
    implicit val stringToLog: StringToLog[M[T]],
    implicit val monad: Monad[M]
) {

  type LOG = M[T]

  case class Writer[A](log: LOG, a: A) {
    def map[B](f: A => B): Writer[B] = Writer(log, f(a))

    def flatMap[B](f: A => Writer[B])/*(implicit acc: Monoid[LOG])*/: Writer[B] = {
      val Writer(log2, b) = f(a)
      Writer(/*acc*/monoid.append(log, log2), b)
    }
  }

  import Endo._

  object Writer {
    implicit def LogUtilities[A](a: A)/*(implicit acc: Monoid[LOG])*/ = new {
      def nolog = Writer(/*acc*/monoid.empty, a)
      def withLog(message: String) = Writer(stringToLog.stringToLog(message), a)
      def withValueLog(mkMessage: A => String) = withLog(mkMessage(a))
    }
  }

  import Writer._

  def go(n: Int) {
    val result: Writer[Boolean] = for {
      a <- n withValueLog ("starting with " + _)
      b <- (a + 7) withLog "adding 7"
      c <- (b * 3).nolog
      d <- c.toString.reverse.toInt withValueLog ("switcheroo with " + _)
      e <- (d % 2 == 0) withLog "is even?"
    } yield e
    println("Result: " + result.a)
    println("LOG")
    println("===")
    fe.foreachPrintln(result.log)
  }

  def main(args: Array[String]) {
    val n = args(0).toInt
    go(n)
  }
}

object Demos {
  def scope(block: =>Unit) {
    block
  }

  def main(args: Array[String]) {
    println("EndoDemo")
    EndoDemo.main(Array())

    println()
    println("WriterDemo")
    WriterDemo.main(Array("41"))

    println()
    println("EndoWriterDemo")
    EndoWriterDemo.main(Array("41"))

    println()
    println("MonoidWriterDemo[List[String]]")
    scope {
      import ListX._
      new MonoidWriterDemo[List, String].main(Array("41"))
    }

    println()
    println("MonoidWriterDemo[Consing[String]]")
    new MonoidWriterDemo[Consing, String].main(Array("41"))

    println()
    println("MonoidWriterDemo[Endo[List[String]]]")
    new MonoidWriterDemo[Endo, List[String]].main(Array("41"))
  }
}

//Demos.main(Array())
