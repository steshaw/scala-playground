import scala.runtime.ScalaRunTime

trait CaseLike extends Product {
  override def toString = ScalaRunTime._toString(this)
  override def hashCode = ScalaRunTime._hashCode(this)
  override def equals(x: Any) = x match {
    case that: Product => that.canEqual(this) && ScalaRunTime._equals(this, that)
    case _ => false
  }
  override def canEqual(x: Any) = x match {
    case that: Product => this.productPrefix == that.productPrefix
    case _ => false
  }
}

class Person(val name: String, val age: Int) extends CaseLike {
  def productArity = 2
  def productElement(i: Int) = if (i == 0) name else if (i == 1) age else error("productElement index out of bounds")
}

val bob1 = new Person("Bob", 25)
val bob2 = new Person("Bob", 25)
val fred = new Person("Fred", 40)

println(bob1)
println(bob2)
println(bob1 == bob2)
println(fred)
println(fred == bob2)
println(bob1 == fred)
