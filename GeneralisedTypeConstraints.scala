//
// Found at http://stackoverflow.com/questions/3427345/what-do-and-mean-in-scala-2-8-and-where-are-they-documented/3427759#3427759
//

case class Foo[A](a:A) { // 'A' can be substituted with any type
  // getStringLength can only be used if this is a Foo[String]
  def getStringLength(implicit evidence: A =:= String) = a.length
}
