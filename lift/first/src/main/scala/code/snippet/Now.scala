package code.snippet

import code.lib._

import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import java.util.Date

object TimeNow {
  //lazy val date: Box[Date] = DependencyFactory.inject[Date]

  //def render = "* *" #> date.map(_.toString)
  def render = "* *" #> now.toString
}
