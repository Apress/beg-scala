import scala.actors.Actor
import Actor._

case object GetMessages
case class Messages(msg: List[String])
case class Remove(who: Actor)
case class Add(who: Actor)

object ChatServer3 extends Actor {
  private var chats: List[String] = Nil
  private var listeners: List[Actor] = Nil

  def act = loop {
    react(calcReact)
  }

  private def calcReact = {
    val handle: PartialFunction[Any, Unit] = {
      case s: String => chats = s :: chats
        notifyListeners()

      case GetMessages => reply(Messages(chats))
    }

    val mgt: PartialFunction[Any, Unit] =
    if (chats.length < 3)
    Map.empty
    else {
      case Add(who) => listeners = who :: listeners
        who ! Messages(chats)

      case Remove(who) => listeners -= who
    }

    handle orElse mgt
  }

  private def notifyListeners() {
    listeners.foreach(a => a ! Messages(chats))
  }

  this.start()
}
