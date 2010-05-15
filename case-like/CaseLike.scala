//
// Adapted from post to scala-debate by Jorge Ortiz:
//   http://scala-programming-language.1934581.n4.nabble.com/Case-classes-and-their-discontents-td2009506.html#a2009511
//

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

  override def productPrefix = this.getClass.getName
  override def productArity = parts.size
  override def productElement(n: Int) = parts(n)

  def parts: Seq[Any]
}
