import scala.actors.Actor
import Actor._
import scala.util.control.Exception.catching

object NameResolver extends Actor {
  import java.net.{InetAddress, UnknownHostException}

  def act() {
    loop {
      react {
        case (name: String, actor: Actor) => {
          val ip = getIp(name)
          printf("ip = %s\n", ip)
          actor ! ip
        }
        case "EXIT" => 
          println("Name resolver exiting.")
          throw new RuntimeException("I'm outta here")
        case msg =>
          printf("Unhandled message %s\n", msg)
      }
    }
  }

  def getIp(name: String): Option[InetAddress] =
    catching(classOf[UnknownHostException]) opt InetAddress.getByName(name)
}
