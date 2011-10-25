
abstract class FunctorModule {
  type Foo

  def foo(f: Foo)
}

object Module extends FunctorModule {
  type Foo = String

  override def foo(f: Foo) = f.length
}

object Main extends App {
  println(Module.foo("hi"))
}
