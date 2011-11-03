
object Email {
  def apply(user: String, domain: String) = user + "@" + domain

  def unapply(s: String): Option[(String, String)] = {
    val parts = s split "@"
    if (parts.length == 2) Some(parts(0), parts(1)) else None
  }
}

val Email(user, domain) = "steshaw@gmail.com"

object Twice {
  def apply(s: String): String = s + s
  def unapply(s: String): Option[String] = {
    val length = s.length / 2
    val half = s.substring(0, length)
    if (half == s.substring(length)) Some(half) else None
  }
}

object UpperCase {
  def unapply(s: String): Boolean = s.toUpperCase == s
}

def userTwiceUpper(s: String) = s match {
  case Email(Twice(x @ UpperCase()), domain) => "match: " + x + " in domain " + domain
  case _ => "no match"
}

object Domain {
  def apply(parts: String*): String = parts.reverse.mkString(".")
  def unapplySeq(whole: String): Option[Seq[String]] = Some(whole.split("""\.""").reverse)
}

def isTomInDotCom(s: String): Boolean = s match {
  case Email("tom", Domain("com", _*)) => true
  case _ => false
}

val tomAtSunCom = isTomInDotCom("tom@sun.com")
val peterAtSunCom = isTomInDotCom("peter@sun.com")
val tomAtAcmOrg = isTomInDotCom("tom@acm.org")

object ExpandedEmail {
  def unapplySeq(email: String): Option[(String, Seq[String])] = {
    val parts = email split "@"
    if (parts.length == 2)
      Some(parts(0), parts(1).split("""\.""").reverse)
    else
      None
  }
}

val ExpandedEmail(name, tomdom, subdoms @ _*) = "tom@support.epfl.ch"
