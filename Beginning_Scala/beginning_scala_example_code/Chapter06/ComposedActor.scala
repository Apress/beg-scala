import scala.actors.Actor
import Actor._

trait Buildable {
  def handler: PartialFunction[Any, Unit] = Map.empty
}

case class Add(who: Actor)
case class Remove(who: Actor)

trait ListenerMgt extends Buildable {
  private var listeners: List[Actor] = Nil

  override def handler = super.handler orElse {
    case Add(who) =>
      listeners = who :: listeners
      who ! updateMessage
    case Remove(who) => listeners -= who
  }

  protected def updateListeners() {
    val m = updateMessage
    listeners.foreach(a => a ! m)
  }

  protected def updateMessage: Any
}

case object GetInfo

trait GetMgt extends Buildable {
  override def handler = super.handler orElse {
    case GetInfo => reply(updateMessage)
  }

  protected def updateMessage: Any
}

case class Messages(msgs: List[String])

object Chat extends Actor with ListenerMgt with GetMgt {
  private var msgs: List[String] = Nil
  def act = loop {
    react(handler orElse {
        case s: String => msgs ::= s
          updateListeners()
      })
  }

  protected def updateMessage = Messages(msgs)

  this.start
}

class Listen extends Actor {
  def act = loop {
    react {
      case Messages(m) => println("Got "+m)
    }
  }

  this.start
}
