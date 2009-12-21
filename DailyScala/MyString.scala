
// See Daily Scala blog

class MyString(val s:String) {
  println("MyString ctor")
  private var extra = ""
  override def toString = s + extra
}

object MyString {
  def apply(base:String, xtra:String) = {
    println("apply/2")
    val result = new MyString(base)
    result.extra = xtra
    result
  }
  def apply(base:String) = {
    println("apply/1")
    new MyString(base)
  }
}
