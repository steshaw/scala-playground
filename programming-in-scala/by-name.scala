
def times(num: Int)(action: => Unit) = for (i <- 1 to num) action

times(3) {
  println("hello")
}

class Times(num: Int) {
  def times(action: => Unit) = for (i <- 1 to num) action
}

implicit def int2times(i: Int) = new Times(i)

3 times {
  println("hi")
}

def loop(action: => Unit) {
  action
  loop(action)
}

loop {
  println("...")
}
