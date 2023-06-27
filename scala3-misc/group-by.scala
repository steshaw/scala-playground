@main
def main =
  val fruits = List("apple", "banana", "avocado", "papaya")

  // count how many 'a' in each fruit
  val countsToFruits = fruits.groupBy(fruit => fruit.count(_ == 'a'))

  for (count, fruits) <- countsToFruits do
    println(s"with 'a' × $count = $fruits")
