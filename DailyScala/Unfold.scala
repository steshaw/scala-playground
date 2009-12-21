
// See DailyScala blog

def unfoldRight[A, B](seed: B)(f: B => Option[(A, B)]): List[A] = f(seed) match {
  case Some((a, b)) => a :: unfoldRight(b)(f)
  case None => Nil
}

def unfoldLeft[A, B](seed: B)(f: B => Option[(B, A)]) = {
  def loop(seed: B)(ls: List[A]): List[A] = f(seed) match {
    case Some((b, a)) => loop(b)(a::ls)
    case None => ls
  }
  loop(seed)(Nil)
}

