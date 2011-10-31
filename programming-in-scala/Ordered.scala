// FIXME: nasty "divering implicit expansion" compiler error.
val o1: Ordered[Int] = 1
val o2: Ordered[Int] = 2
println(o1 < o2)
