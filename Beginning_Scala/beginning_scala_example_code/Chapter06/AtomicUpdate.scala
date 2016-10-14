import scala.actors.Actor
import Actor._

case object GetInfo
case class Info(i: Map[String, Int])
case class SetInfo(n: String, v: Int)
case class Update(n: String, f: Option[Int] => Int)

object XAct1 extends Actor {
  private var info: Map[String, Int] = Map()

  def act = loop {
    react {
      case GetInfo => reply(Info(info))
      case SetInfo(n, v) => info += n -> v
      case Update(n, f) => info += n -> f(info.get(n))
    }
  }

  this.start
}
