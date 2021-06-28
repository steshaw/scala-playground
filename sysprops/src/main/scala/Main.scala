package object fred {
  def inspect(s: String) = {
    import scala.reflect.runtime.universe._
    Literal(Constant(s)).toString
  }
}

object Main extends App {
  println("Hello, Nottingham!")
  println(s"Scala version = ${util.Properties.versionString}")
  println("System Properties {")
  sys.props.toList.sorted.foreach{case (name, value) =>
    println(s"  ${name}: ${fred.inspect(value)}")
  }
  println("}")
}
