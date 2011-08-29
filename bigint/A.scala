object A extends App {
  def plusOne(i: BigInt) = i + 1

  def bigIntImplicit1 = plusOne(1)
  def bigIntImplicit2 = {
    val i: Int = 10
    plusOne(i)
  }
}
