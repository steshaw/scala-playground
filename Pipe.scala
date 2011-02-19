
object Pipe {
  implicit def toPipe[T](x : T) = new {
    def |> [U](f: T => U) = f(x)
  }
}
