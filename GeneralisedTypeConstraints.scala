//
// Adapted from http://stackoverflow.com/questions/3427345/what-do-and-mean-in-scala-2-8-and-where-are-they-documented/3427759#3427759
//

implicit def int2string(i: Int) = i.toString

case class Foo[A](a:A) { // 'A' can be substituted with any type
  // stringLength can only be used if this is a Foo[String]
  def stringLength(implicit evidence: A =:= String) = a.length

  // stringLengthSubtype can be used if A is a subtype of CharSequence such as Foo[String]
  def stringLengthSubtype(implicit evidence: A <:< CharSequence) = a.length

  // stringLengthView can be used if A is implicitly convertible to a CharSequence, 
  // such as Foo[Int], given int2string above
  def stringLengthView(implicit evidence: A => String) = a.length
}
