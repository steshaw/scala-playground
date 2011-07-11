//
// See http://moreindirection.blogspot.com/2011/07/prefer-recursion-to-var-and-while-loops.html
//

import scala.annotation.tailrec

object ReadUtil {
  def readUntil[X](prompt: String, transform: String => X, pred: X => Boolean): X =  {
    val reader = new jline.ConsoleReader() { setDefaultPrompt(prompt)}

    @tailrec def ruHelper: X = Option(reader.readLine()).map(transform(_)) match {
      case Some(x) if pred(x) => x
      case _ => ruHelper
    }

    ruHelper
  }

  def readUntil(prompt: String, pred: String => Boolean): String =  {
      readUntil(prompt, x => x, pred)
  }

  def main(args: Array[String]) = readUntil("non-blank> ", _.length > 0)
}
