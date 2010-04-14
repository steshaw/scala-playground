
//object Foo extends Logging

class Foo extends Logging {
  log.debug("when does this happen?")

  def blah_blah() {
    log.debug("started blah_blah")
  }
}

object Main {
  def main(args: Array[String]) = 
    println("{{{")
    val f = new Foo
    f.blah_blah
    f.blah_blah()
    println("}}}")
}
