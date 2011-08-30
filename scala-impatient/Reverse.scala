object Reverse {
  def reverseByFoldLeft[T](l: List[T]): List[T] = l.foldLeft(List():List[T])((l: List[T], e: T) => e :: l)

  def reverseByFoldLeft2[T](l: List[T]) = l.foldLeft(List():List[T])((l: List[T], e: T) => e :: l)

  def reverseByFoldLeft3[T](l: List[T]) = l.foldLeft(List():List[T])((l, e) => e :: l)
}
