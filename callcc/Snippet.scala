/**
 * Attempt to simulate the following Scheme snippet:
 *
 *    (+ 1 (call/cc
 *     (lambda (k)
 *        (+ 2 (k 4)))))
 *
 */
object Snippet {
  def main(args: Array[String]) {
    println("hello world")
  }
}
