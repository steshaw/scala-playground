object CubeCalculator {
  def cube(x: Int) = {
    x * x * x
  }
  def main(args: Array[String]) = {
    println(s"Java VM vendor = ${sys.props("java.vm.vendor")}")
    println(s"Java version = ${sys.props("java.version")}")
    println(s"Scala version: ${util.Properties.versionString}")
    println(s"cube(3) = ${cube(3)}")
  }
}
