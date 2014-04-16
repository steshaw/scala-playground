
case class OrderedList[A] private(xs: List[A]) {
  // this should be O(n) worst case but is O(n log n) to avoid violating constraint and/or losing data
  def +(a: A)(implicit O: Ordering[A]) =
    (a :: xs).sorted
 
  // this should be O(n) in length of only one of the lists (but is O(m + n log n))
  // this could also lose data if the equality is inconsistent, reguardless of extra sort
  def ++(a: OrderedList[A])(implicit O: Ordering[A]) =
    (xs ++ a.xs).sorted
}
