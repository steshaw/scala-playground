trait CheckThisForNull {
  def checkThisForNull() = if (this eq null) "null" else "some"
}

class Thing extends CheckThisForNull

object ThisNull {

  def main(args: Array[String]): Unit = {

    println(new Thing().checkThisForNull())

    val a: CheckThisForNull = null
    println(a.checkThisForNull())
  }
}
