object CubeCalculator extends App {
  def cube(x: Int) = {
    x * x * x
  }
  def main() = {
    println(s"cube(3) = ${cube(3)}")
  }
  main()
}
