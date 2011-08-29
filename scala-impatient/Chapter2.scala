object Chapter2 extends App {
  def sum(args: Int*) = {
    var result = 0
    for (arg <- args) result += arg
    result
  }

  println(sum(1,2,3))

  println(sum(1 to 3: _*))

  val a = List(1,2,3)

  println(sum(a: _*))

  println(sum(args.map(_.toInt): _*))

  def countdown(n: Int) = (0 to n).reverse foreach (println)

  val s = "Hello"

  {
    var p = 1
    for (c <- s) p *= c.toInt
    printf("Product unicodes for %s is %d\n", s, p)

    printf("s.product.toInt 1 - %d\n", s.product.toInt)
    printf("s.product :Int - %d\n", s.product :Int)
  }

  def product(s: String): Int = s.product
  printf("product - %d\n", product(s))

  def mulChar(a: Char, b: Char):Char = (a * b).toChar

  def productByReduce(s: String): Int = s.reduce(mulChar).toInt
  printf("productByReduce - %d\n", productByReduce(s))

  def productByFold(s: String): Int = s.fold(1.toChar)(mulChar)
  printf("productByFold - %d\n", productByFold(s))

  def productByFoldLeft(s: String): Int = s.foldLeft(1)(_ * _)
  printf("productByFoldLeft - %d\n", productByFoldLeft(s))

  def productByFoldRight(s: String): Int = s.foldRight(1)(_ * _)
  printf("productByFoldRight - %d\n", productByFoldRight(s))

  def productRecursive(s: String): Int =
    if (s.isEmpty) 1
    else s.head * productRecursive(s.tail)

  printf("productRecursive - %d\n", productRecursive(s))

  def productTailRecursive(s: String): Int = {
    def aux(acc: Int, s:String): Int = if (s.isEmpty) acc else aux(acc * s.head, s.tail)
    aux(1, s)
  }

  printf("productTailRecursive - %d\n", productTailRecursive(s))

  def power(x: Double, n: Int): Double =
    if (n > 0)
      if (n % 2 == 0) {
        val y = power(x, n / 2)
        y * y
      } else x * power(x, n - 1)
    else if (n == 0) 1
    else 1 / power(x, -n)

}
