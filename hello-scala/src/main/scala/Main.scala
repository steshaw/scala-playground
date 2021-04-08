package object fred {
  def escape(s: String) = s.replaceAllLiterally("\n", "\\n")
}

object Main extends App {
  println("Hello, Nottingham!")
  println(s"Scala version = ${util.Properties.versionString}")
  println("System Properties {")
  sys.props.toList.sorted.foreach{case (name, value) =>
    val q = "\""
    println(s"  ${name}: ${q}${fred.escape(value)}${q}")
  }
  println("}")
}
