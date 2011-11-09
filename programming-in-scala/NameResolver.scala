import scala.actors.Actor

object NameResolver extends Actor {
  import java.net.{InetAddress, UnknownHostException}

  def act() {
    react {
      case (name: String, actor: Actor) => 
        actor ! getIp(name)
        act()
      case "EXIT" => 
        println("Name resolver exiting.")
      case msg =>
        printf("Unhandled message %s\n", msg)
        act()
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
