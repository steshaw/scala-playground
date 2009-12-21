
def factorial_loop(i: BigInt): BigInt = {
  var result = BigInt(1)
  for (j <- 2 to i.intValue)
    result *= j
  result
}

def factorial(i: BigInt): BigInt = {
  def fact(i: BigInt, acc: BigInt): BigInt =
    if (i == BigInt(1)) acc
    else fact(i - 1, i * acc)
  fact(i, 1)
}

for (i <- 1 to 10) {
  Console.printf("%2d: %s\n", i, factorial_loop(i))
  Console.printf("%2d: %s\n", i, factorial(i))
}
