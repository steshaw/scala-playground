def isEven(i: Int) = i % 2

object Fn:
  def main(args: Array[String]) =
    // Apparently, you can't "store a function in a variable" in Kotlin.
    // Supposedly, that is connect with "how the JVM works under the hood".
    val f = isEven
    println(f(1))
    println(f(2))
