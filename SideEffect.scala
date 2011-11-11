object SideEffect {
  //
  // Neither of these methods have side effects - filterNot returns a filtered list.
  // However, I'm checked to see if there is a compiler warning about the ignored
  // return value in either case. Answer: Not in Scala 2.9.1.
  //
  def sideEffect(as: List[Int]): Unit = {
    as filterNot (_ == 1)
  }
  def sideEffect2(as: List[Int]) {
    as filterNot (_ == 1)
  }
}
