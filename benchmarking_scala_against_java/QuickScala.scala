object QuickScala {
  def sortArray(array: Array[Int], left: Int, right: Int) {
    if (right <= left) {
      return
    }
    val pivot = array(right)
    var p = left
    var i = left
    while (i < right) {
      if (array(i) < pivot) {
        if (p != i) {
          val tmp = array(p)
          array(p) = array(i)
          array(i) = tmp
        }
        p += 1
      }
      i += 1
    }
    array(right) = array(p)
    array(p) = pivot
    sortArray(array, left, p - 1)
    sortArray(array, p + 1, right)
  }
}
