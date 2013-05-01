/* 
 * Scala port of Erik Meijer's slide on "Lazy evaluation and side-effects"
 * from OSCON 2009 keynote titled "Fundamentalist Functional Programming".
 */

def lessThanThirty(x: Int) = {
  println(s"${x}? less than 30")
  x < 30
}

def moreThanTwenty(x: Int) = {
  println(s"${x}? more than 20")
  x > 20
}

val q0 = for {
           x <- Array(1, 25, 40, 5, 23).toStream // note the .toStream
           if lessThanThirty(x)
         } yield x

val q1 = for {
           x <- q0
           if moreThanTwenty(x)
         } yield x

q1 foreach (x => println(s"[$x]"))
