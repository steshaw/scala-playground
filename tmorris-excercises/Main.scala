//
// From http://blog.tmorris.net/monad-exercises-in-scala-addendum/
//
// MonadExcercises test harness 
//

object Main {
  def main(args: Array[String]) {
    import Monad._
    import MonadicFunctions._
 
    val plusOne = Inter(1+)
    val multTwo = Inter(2*)
    val squared = Inter(n => n*n)
    val plus = (_: Int) + (_: Int)
 
    val values = List(
// sequence
sequence(List(List(1, 2), List(3, 4)), ListMonad),
sequence(List(Some(7), Some(8), Some(9)), OptionMonad),
sequence(List(Some(7), None, Some(9)), OptionMonad),
sequence(List(plusOne, multTwo, squared), InterMonad) f 7,
sequence(List(Identity(7), Identity(4)), IdentityMonad),
// fmap
fmap(List(1, 2, 3), (x: Int) => x * 10, ListMonad),
fmap(Some(8), (x: Int) => x * 10, OptionMonad),
fmap(None: Option[Int], (x: Int) => x * 10, OptionMonad),
fmap(plusOne, (x: Int) => x * 10, InterMonad) f 7,
fmap(Identity(9), (x: Int) => x * 10, IdentityMonad),
// flatten
flatten(List(List(1, 2), List(3, 4)), ListMonad),
flatten(Some(Some(8)), OptionMonad),
flatten(Some(None: Option[Int]), OptionMonad),
flatten(None: Option[Option[Int]], OptionMonad),
flatten(Inter(a => Inter(a *)), InterMonad) f 7,
flatten(Identity(Identity(8)), IdentityMonad),
// apply
apply(List((a: Int) => a + 1,
           (a: Int) => a * 2,
           (a: Int) => a % 2), List(1, 2, 3), ListMonad),
apply(Some((a: Int) => a + 1), Some(8), OptionMonad),
apply(None: Option[Int => Int], Some(8), OptionMonad),
apply(Some((a: Int) => a + 1), None: Option[Int], OptionMonad),
apply(Inter(a => (b: Int) => a * b), Inter(1+), InterMonad) f 7,
apply(Identity((a: Int) => a + 1), Identity(7), IdentityMonad),
// filterM
filterM((a: Int) => List(a > 2, a % 2 == 0), List(1, 2, 3), ListMonad),
filterM((a: Int) => Some(a > 1), List(1, 2, 3), OptionMonad),
filterM((a: Int) => Inter(n => a * n % 2 == 0),
  List(1, 2, 3), InterMonad) f 7,
filterM((a: Int) => Identity(a > 1), List(1, 2, 3), IdentityMonad),
// replicateM
replicateM(2, List(7, 8), ListMonad),
replicateM(2, Some(7), OptionMonad),
replicateM(2, plusOne, InterMonad) f 7,
replicateM(2, Identity(6), IdentityMonad),
// lift2
lift2(plus, List(1, 2), List(3, 4), ListMonad),
lift2(plus, Some(7), Some(8), OptionMonad),
lift2(plus, Some(7), None: Option[Int], OptionMonad),
lift2(plus, None: Option[Int], Some(8), OptionMonad)
    )
 
    val verify = List(
// sequence
List(List(1, 3), List(1, 4), List(2, 3), List(2, 4)),
Some(List(7, 8, 9)),
None,
List(8, 14, 49),
Identity(List(7, 4)),
// fmap
List(10, 20, 30),
Some(80),
None,
80,
Identity(90),
// flatten
List(1, 2, 3, 4),
Some(8),
None,
None,
49,
Identity(8),
// apply
List(2, 3, 4, 2, 4, 6, 1, 0, 1),
Some(9),
None,
None,
56,
Identity(8),
// filterM
List(List(3), Nil, List(2, 3), List(2), List(3),
  Nil, List(2, 3), List(2)),
Some(List(2, 3)),
List(2),
Identity(List(2, 3)),
// replicateM
List(List(7, 7), List(7, 8), List(8, 7), List(8, 8)),
Some(List(7, 7)),
List(8, 8),
Identity(List(6, 6)),
// lift2
List(4, 5, 5, 6),
Some(15),
None,
None
)
 
    for((a, b) <- values zip verify)    
      println(if(a == b) "PASS"
              else "FAIL. Expected: " + b + " Actual: " + a)
  }
}
