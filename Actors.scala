//
// See http://daily-scala.blogspot.com/2009/11/introduction-to-actors.html
//

import scala.actors.Actor
import Actor._

val collector = actor {
  var count = 3
  var data = ""
  loop {
    react {
      case payload:String => {
        reply("thank you")
        data += payload + "\n\n"
        count -= 1
        if (count == 0) {
          println(data)
          exit()
        }
      }
    }
  }
}

class Downloader(url:String) extends Actor {
  def act = {
    import scala.io.Source
    val source = Source.fromURL(new java.net.URL(url))
    val data = source.getLines("\n").mkString("\n")
    collector ! data
    receive { case s => println("Done with "+url) }
  }
}

val urls = List(
    "http:/daily-scala.blogspot.com/2009/11/using-objects-as-functions.html",
    "http:/daily-scala.blogspot.com/2009/10/boolean-extractors.html",
    "http:/daily-scala.blogspot.com/2009/08/java-vs-scala-control-structures.html",
    "http:/www.blogger.com/profile/07600430363435495915")

println("get started")
println(urls)
for (url <- urls) {
  new Downloader(url).start
}

link(collector)
