package code.snippet

import net.liftweb._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._

class ClickMe {
  object pos extends RequestVar(0)
  object count extends RequestVar(0)

  def render = {
    printf("\n\nrender %s\n\n", (pos, count, pos.is, count.is))
    val posOnPage = pos.set(pos.is + 1)
    "button [onclick]" #> SHtml.ajaxInvoke(() => {
      printf("\n\ninvoke: before ++count - %s\n\n", (pos, count, pos.is, count.is))
      count.set(count.is + 1)
      printf("\n\ninvoke:  after ++count - %s\n\n", (pos, count, pos.is, count.is))
      Alert("Thanks pos: %s posOnPage: %s count: %s".format(pos, posOnPage, count.is))
    })
  }
}
