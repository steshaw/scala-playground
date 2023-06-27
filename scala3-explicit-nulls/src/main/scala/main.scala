//
// scala -Yexplicit-nulls main.scala
//

extension [T](x: T | Null)
   inline def nn: T =
     assert(x != null)
     x.asInstanceOf[T]

def foo(i : Int) =
  println(s"foo: i = $i")

object ExplicitNulls:
  @main def main =
    val a : Int | Null = null
    //foo(a) // Will not compile.
    if (a != null) {
      foo(a) // Compiles presumably because of flow-based type-checking.
    }
    foo(a.nn) // Assertion failure!
