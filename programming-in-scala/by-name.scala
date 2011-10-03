
def times(num: Int)(action: => Unit) = for (i <- 1 to num) action

times(3) {
  println("hello")
}
