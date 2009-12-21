//
// See http://lachlanrambling.blogspot.com/2009/12/some-simple-scala-null-tricks.html
//

def foo(i:Int):String = if (i == 0) null else i.toString

def ?[A <: AnyRef](ref: A): Option[A] = 
  if (ref eq null) None else Some(ref)

for (i <- -5 to 5) {
  val value = ?(foo(i)).getOrElse("")
  print("value is '")
  print(value)
  println("'");
}

object NotNull {
  def unapply[A <: AnyRef](ref: A): Option[A] =
    if (ref eq null) None else Some(ref)
}

val xs = List("a", null, "b", null, null, "c")

println(for (NotNull(x) <- xs) yield x)
