import scala.actors.Actor
import Actor._

object NameResolver extends Actor {
  import java.net.{InetAddress, UnknownHostException}

  def act() {
    loop {
      react {
        case (name: String, actor: Actor) => 
          actor ! getIp(name)
        case "EXIT" => 
          println("Name resolver exiting.")
          throw new RuntimeException("I'm outta here")
        case msg =>
          printf("Unhandled message %s\n", msg)
      }
    }
  }

  def getIp(name: String): Option[InetAddress] = {
    try {
      Some(InetAddress.getByName(name))
    } catch {
      case _:UnknownHostException => None
    }
  }
}
