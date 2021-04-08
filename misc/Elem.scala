//
// Reading discussion here http://lambda-the-ultimate.org/node/4377#comment-67603,
// I was quite surprised that the signature of List.contains is: def contains(item: Any): Boolean.
// This allows:
//
val l = List(1, 2, 3)

println(l contains 1)
println(l contains 'a')
println(l contains "one")

//
// Even if you define 'elem' like this,
//
def elem[T, E >: T](e: E, l: List[T]) = l contains e

//
// ... these are still possible. T then is resolved to Any :(.
//
println(elem(1, l))
println(elem('a', l))
println(elem("one", l))
