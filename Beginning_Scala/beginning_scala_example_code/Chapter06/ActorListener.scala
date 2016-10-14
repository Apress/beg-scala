import scala.actors.Actor
import Actor._

case class Add(who: Actor)
case class Changed(what: AFoo, count: Int)
case object Access

class AFoo extends Actor {
  private var listeners: List[Actor] = Nil
  private var count = 0

  def act = loop {
    react {
      case Add(who) => listeners = who :: listeners
      case Access => access()
    }
  }

  private def access() = {
    notifyListeners
    count += 1
  }

  private def notifyListeners =
  listeners.foreach(a => a ! Changed(this, count))
}

class AFooListener(afoo: AFoo) extends Actor {
  afoo ! Add(this)

  def act = loop {
    react {
      case Changed(f, cnt) => changed(f, cnt)
    }
  }

  def changed(eventFrom: AFoo, count: Int): Unit = {
    if (count < 10) eventFrom ! Access
  }
}
