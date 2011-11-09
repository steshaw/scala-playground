//
// Dodgy workaround for 'self.receiveWithin(0) case { a => a }' not working from the REPL.
//

import scala.actors.Actor
import Actor._

object DebugActor extends Actor {
  def act {
    loop {
      react {
        case ("tryReceive", actor: Actor) =>
          val msg = receiveWithin(0) { case a => a }
          printf("DebugActor received %s\n", msg)
          actor ! msg
        case "EXIT" => throw new RuntimeException("DebugActor EXIT")
      }
    }
  }
}

object Debug {
  var msg: Any = null
  def message = msg
  def sendSelf() {
    NameResolver ! ("steshaw.org", self) // XXX: self?
  }
  def sendDebugActor() {
    NameResolver ! ("steshaw.org", DebugActor)
  }
  def tryReceive(): Any = {
    actor {
      DebugActor ! ("tryReceive", self)
      receive {
        case m =>
          printf("Debug.tryReceive received %s\n", m)
          msg = m
      }
    }
  }
}
