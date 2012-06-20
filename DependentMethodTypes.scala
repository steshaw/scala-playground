//
// Example from http://www.scala-lang.org/node/11415
//
// Compile with scalac -Ydependent-method-types
//

trait Foo {
  type Arg
  type Prod
  def makeProd(a: Arg): Prod
}

object Test {
  def f1(x: Foo)(y: x.Arg) = x.makeProd(y)

  case class f2(x: Foo) {
    def apply(y: x.Arg) = x.makeProd(y)
  }

  // Miles Sabin's work-around.
  case class f3[T <: Foo](x: T) {
    def apply(y: x.Arg) = x.makeProd(y)
  }

  val myFoo = new Foo {
    type Arg = Int
    type Prod = (Int, Int)
    def makeProd(i: Int) = (i, i)
  }

  def main(args: Array[String]): Unit = {
    println(f1(myFoo)(5)) // works

    //println(f2(myFoo)(10)) // nope
/* Compile error:
DependentMethodTypes.scala:23: error: type mismatch;
 found   : Int(10)
 required: _1.x.Arg where val _1: Test.f2
    println(f2(myFoo)(10)) // nope
                      ^
one error found
*/
    println(f3(myFoo)(10)) // using Miles' work-around
  }
}
