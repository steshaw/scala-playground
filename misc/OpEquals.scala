case class Foo(private val i: Int, private val j: Int) {
  def +|+(other: Foo) = Foo(i + other.i, j + other.j)
}

var f = Foo(0, 1)
f +|+= Foo(2, 3)
f +|+= Foo(1, 5)

println(f)
