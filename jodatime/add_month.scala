//
// See what happens when 'add one month' is applied to a date at the end of the month. ie. 2009-12-31.
// What happens is that it hits February which gets 2010-02-28 and then always the 28th is returned.
// Similar "problems" would happend when passing through a month with the 30th.
//

import org.joda.time._

def imperativeStyle() {
  var dt = new DateTime("2009-12-31")
  def go() {
    (1 to 13).foreach {(n:Int) =>
      println(n)
      dt = dt.plusMonths(1)
      println(dt.toString())
    }
  }
  go()
}

def functionalStyle() {
  def iter(i: Int, dt: DateTime) {
    if (i <= 13) {
      println(i)
      val newDt = dt.plusMonths(1)
      println(newDt.toString())
      iter(i+1, newDt)
    }
  }
  iter(1, new DateTime("2009-12-31"))
}

def lazyFunctionalStyle() {
  Stream.from(1) zip Stream.iterate(new DateTime("2009-12-31"))(_.plusMonths(1)).drop(1).take(13) foreach {
    case (n, dt) => 
      println(n)
      println(dt)
  }
}
