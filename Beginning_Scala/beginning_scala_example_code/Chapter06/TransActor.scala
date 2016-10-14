import scala.actors.Actor
import Actor._
import scala.actors.TIMEOUT

case object GetInfo
case class Info(i: Map[String, Int])
case class SetInfo(n: String, v: Int)
case class Update(n: String, f: Option[Int] => Int)
case class BeginXAction(id: Int)
case object CommitXAction
case object RollbackXAction

class Acct extends Actor {
  private var info: Map[String, Int] = Map()
  private var service = (normal, false)

  private def normal: PartialFunction[Any, Unit] = {
    case GetInfo => reply(Info(info))
    case SetInfo(n, v) => info += n -> v
    case Update(n, f) => info += n -> f(info.get(n))
    case BeginXAction(id) => begin(id)
  }

  private def begin(xActionId: Int) {
    val oldInfo = info // capture
    val oldService = service
    val tmp: PartialFunction[Any, Unit] = {
      case TIMEOUT => // Rollback
        info = oldInfo
        service = oldService

      case (n, RollbackXAction) if n == xActionId =>
        info = oldInfo
        service = oldService

      case (n, CommitXAction) if n == xActionId => // Commit
        service = oldService

      case (n, v) if n == xActionId &&
        normal.isDefinedAt(v) => normal(v)
    }

    service = (tmp, true)
  }

  def act = loop {
    service match {
      case (pf, false) => react(pf)
      case (pf, true) => reactWithin(500)(pf)
    }
  }

  this.start
}
object TestAcct {
  def doTest() = {
    val dpp = new Acct
    dpp ! Update("Savings", v => (v getOrElse 0) + 1000)
    dpp ! Update("Checking", v => (v getOrElse 0) + 100)

    val archer = new Acct
    archer ! Update("Savings", v => (v getOrElse 0) + 2000)
    archer ! Update("Checking", v => (v getOrElse 0) + 50)

    println("Initial balances:")
    println("dpp: "+(dpp !? GetInfo))
    println("archer: "+(archer !? GetInfo))

    var xid = 1

    def transfer(who: Actor, from: String, to: String, amount: Int): Boolean = {
      xid += 1

      who ! BeginXAction(xid)
      who !? (500, (xid, GetInfo)) match {
        case Some(Info(bal)) =>
          if (bal.getOrElse(from, 0) > amount) {
            who ! (xid, Update(from, v => (v getOrElse 0) - amount))
            who ! (xid, Update(to, v => (v getOrElse 0) + amount))
            who ! (xid, CommitXAction)
            true
          } else {
            who ! (xid, RollbackXAction)
            false
          }
        case _ => who ! (xid, RollbackXAction)
          false
      }
    }

    transfer(dpp, "Savings", "Checking", 700)
    println("xfer 1 dpp: "+(dpp !? GetInfo))
    transfer(dpp, "Savings", "Checking", 700)
    println("xfer 2 dpp: "+(dpp !? GetInfo))

    def transfer2(src: Actor, sact: String, dest: Actor,
                  dact: String, amount: Int): Boolean = {
      xid += 1

      src ! BeginXAction(xid)
      dest ! BeginXAction(xid)
      (src !? (500, (xid, GetInfo)), dest !? (500, (xid, GetInfo))) match {
        case (Some(Info(sbal)), Some(Info(dbal))) =>
          dest ! (xid, Update(dact, v => (v getOrElse 0) + amount))

          if (sbal.getOrElse(sact, 0) > amount) {
            src ! (xid, Update(sact, v => (v getOrElse 0) - amount))
            src ! (xid, CommitXAction)
            dest ! (xid, CommitXAction)
            true
          } else {
            src ! (xid, RollbackXAction)
            dest ! (xid, RollbackXAction)
            false
          }
        case _ =>
          src ! (xid, RollbackXAction)
          dest ! (xid, RollbackXAction)
          false
      }
    }

    transfer2(dpp, "Checking", archer, "Checking", 700)
    println("XFer 700 dpp -> archer:")
    println("dpp: "+(dpp !? GetInfo))
    println("archer: "+(archer !? GetInfo))

    transfer2(dpp, "Checking", archer, "Checking", 700)
    println("Again, XFer 700 dpp -> archer:")
    println("dpp: "+(dpp !? GetInfo))
    println("archer: "+(archer !? GetInfo))

    transfer2(dpp, "Checking", archer, "Checking", 10)
    println("XFer 10 dpp -> archer:")
    println("dpp: "+(dpp !? GetInfo))
    println("archer: "+(archer !? GetInfo))
  }
}
