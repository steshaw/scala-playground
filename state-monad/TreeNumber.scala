object TreeNumber {

  sealed abstract class Tree[+A] {
    def number(seed: Int): (Tree[(Int)], Int) = this match {
      case Leaf(n) => (Leaf(seed), seed+1)
      case Branch(l, r) => {
        val (lt, seed1) = l.number(seed)
        val (rt, seed2) = r.number(seed1)
        (Branch(lt, rt), seed2)
      }
    }
  }
  final case class Leaf[A](a: A) extends Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def main(args: Array[String]) = {
    def go(tree: Tree[Int]) {
      println(tree)
      println(tree.number(1)._1)
    }

    go(Branch(Branch(Leaf(2), Leaf(6)), Leaf(9)))
    go(Branch(Branch(Leaf(2), Branch(Leaf(4), Leaf(5))), Leaf(9)))
  }

}
