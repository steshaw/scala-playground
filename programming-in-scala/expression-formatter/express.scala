import org.stairwaybook.expr._

object Express extends App {

  val e1 = BinaryOp("*", BinaryOp("/", Number(1), Number(2)),
                         BinaryOp("+", Var("x"), Number(1)))
  val e2 = BinaryOp("+", BinaryOp("/", Var("x"), Number(2)),
                         BinaryOp("/", Number(1.5), Var("x")))
  val e3 = BinaryOp("/", e1, e2)

  val se1 = BinaryOp("-", Number(1),
                          BinaryOp("-", Number(8), Number(7)))
  val se2 = BinaryOp("-", BinaryOp("-", Number(1), Number(8)),
                          Number(7))
  val se3 = BinaryOp("+", BinaryOp("+", Number(1), Number(2)),
                          Number(3))
  val se4 = BinaryOp("+", Number(1),
                          BinaryOp("+", Number(2), Number(3)))

  val f = new ExprFormatter
  def show(e: Expr) = println(f.format(e) + "\n\n")

  val all = Seq(e1, e2, e3, se1, se2, se3, se4)
  all foreach show
}
