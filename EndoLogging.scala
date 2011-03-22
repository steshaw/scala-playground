
case class Endo[A](a: A => A)

def log(message: String): Endo[List[String]] = Endo(as => message::as)

def collapse[A](a: List[Endo[A]]) = a.foldRight(Endo(id[A]))  { case (Endo(a), Endo(b)) => Endo(a compose b) }

def id[A](a: A) = a

val logging = List(log("hi"), log("there"))

val endoResult: Endo[List[String]] = collapse(logging)

val result: List[String] = endoResult match { case Endo(f) => f(List.empty) }

result foreach println
