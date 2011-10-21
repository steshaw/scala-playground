
println("hi")

class Foo extends Enumeration { val A, B, C = Value }

object Foo extends Foo

println(Foo.A)
println(Foo.B)
println(Foo.C)

println(Foo.A.id)
println(Foo.B.id)
println(Foo.C.id)

object Foo2 extends Enumeration { val A, B, C = Value }

println(Foo2.A)
println(Foo2.B)
println(Foo2.C)

println(Foo2.A.id)
println(Foo2.B.id)
println(Foo2.C.id)
