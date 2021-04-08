
def repeatIt(action: => Unit, condition: => Boolean) {
  action
  if (!condition) repeatIt(action, condition)
  else ()
}

{
  println("RepeatSimple")
  var i = 0
  repeatIt({
    i += 1
    println(i)
  }, i >= 5)
  println()
}

trait Until {
  def until(condition: => Boolean): Unit
}

def repeat(action: => Unit): Until = new Until {
  def until(condition: => Boolean) {
    action
    if (!condition) until(condition)
    else ()
  }
}

{
  println("Repeat ... Until")
  var i = 0
  repeat {
    i += 1
    println(i)
  } until (i >= 5)
}
