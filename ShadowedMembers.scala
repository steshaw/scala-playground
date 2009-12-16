
// See http://daily-scala.blogspot.com/2009/12/danger-shadowed-member-values.html

class X(val i:Int)

// The following no longer compiles in Scala 2.8 (my nightly version anyhow). Needs 'override' so maybe shadowing no longer possible.
class Y(val i:Int) extends X(19) {
  println(i)
}

