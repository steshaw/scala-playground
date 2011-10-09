import org.stairwaybook.expr._

object Express extends App {
  val f = new ExprFormatter
  val e1 = BinaryOp("*", BinaryOp("/", Number(1), Number(2)),
                         BinaryOp("+", Var("x"), Number(1)))
  val e2 = BinaryOp("+", BinaryOp("/", Var("x"), Number(2)),
                         BinaryOp("/", Number(1.5), Var("x")))
  val e3 = BinaryOp("/", e1, e2)

  def show(e: Expr) = println(f.format(e) + "\n\n")

  for (e <- Array(e1, e2, e3)) show(e)
}
