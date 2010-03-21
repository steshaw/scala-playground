import scalax.logic._
import scalax.logic.MiniKanren._

object MiniKanrenExample extends Application {
  val c:Var = fresh

  val k = runs('q, 
              Eq(c, 'b),
              Eq('q, c),
              Conde(Eq('b, 10)))

  for (val r <- k.lst) Console.println("Q -> " + r)
}
