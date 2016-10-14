object Random extends Random

trait Creature[BaseType] {
  this: BaseType with Creature[BaseType] =>
  import Creature._

  type Us = BaseType with Creature[BaseType]

  def life: Int
  def strength: Int
  def charisma: Int
  def weapon: Int

  def setLife(life: Int): Us

  protected def rand(max: Int) = Random.nextInt(max)

  def dead_? = life == 0

  protected case class Weapon(turn: (Us, Them) => (Us, Them)) {
    private var _tilDeath = false
    private def this(f: (Us, Them) => (Us, Them), s: Boolean) = {
      this(f)
      _tilDeath = s
    }
    def tilDeath = new Weapon(turn, true)

    def apply(t: Them): (Us, Them) with Rebattle = apply(Creature.this, t)

    private[Creature] def apply(seed: Us, t: Them) = {
      val ret = doBattle(seed, t)
      ret match {
        case (nu, nt) if nu.dead_? =>
          println("You're dead: "+seed+" -> "+nu)
          println("Enemy: "+t+" -> "+nt)
        case (nu, nt) if nt.dead_? =>
          println("You won: "+seed+" -> "+nu)
          println("Enemy: "+t+" -> "+nt)
        case _ =>
      }
      new Tuple2[Us, Them](ret._1, ret._2) with Rebattle
    }

    private def doBattle(u: Us, t: Them): (Us, Them) =
    (u.dead_?, t.dead_?) match {
      case (true, _) | (_, true) => (u, t)
      case _ if !_tilDeath => turn(u, t)
      case _ => turn(u,t) match {case (u2,t2) => doBattle(u2, t2)}
    }
  }

  trait Rebattle { this: (Us, Them) =>
    def apply(w: Weapon) = w(_1, _2)
  }

  protected def round(u: Us, t: Them, damage: Int): (Us, Them) =
  t attackedBy u.calcHit(damage) match {
    case e if e.dead_? => (u, e)
    case e => (u attackedBy t.calcHit(e.weapon), e)
  }

  private def attackedBy(damage: => Int): Us =
  calcLife(u => (u.life - damage) match {
      case n if n <= 0 => u.setLife(0)
      case n => u.setLife(n)
    })

  private def calcLife[T](f: Us => T): T =
  rand(charisma) match {
    case up if up % 9 == 7 =>
      println(this+" magick powers up "+(up / 4))
      f(setLife(life + up / 4))
    case _ => f(this)
  }

  private def calcHit(damage: Int) = {
    val hit = rand(strength + damage)
    println(this+" hits with "+hit+" points of damage!")
    hit
  }

  def >*< = Weapon((u, t) => round(u, t, u.weapon))
}

object Creature {
  type Them = Creature[_]

  implicit def fromSeq(in: Seq[Them]): Them =
    CreatureCons(in.firstOption, in.drop(1))

  case class CreatureCons(head: Option[Them], tail: Seq[Them]) extends
  Creature[CreatureCons] {
    def setLife(n: Int) =
    if (n <= 0) CreatureCons(tail.firstOption, tail.drop(1))
    else CreatureCons(head.map(_.setLife(n)), tail)

    def life = head.map(_.life) getOrElse 0
    def strength = head.map(_.strength) getOrElse 0
    def charisma = head.map(_.charisma) getOrElse 0
    def weapon = head.map(_.weapon) getOrElse 0

    override def toString = "["+ (head.map(_.toString) getOrElse "")+
    (if (tail.isEmpty) "" else "..." + tail.last) + "]"
  }
}

case class Rabbit(life: Int, bombs: Int) extends Creature[Rabbit] {
  val strength = 2
  val charisma = 44
  val weapon = 4

  def setLife(life: Int) = new Rabbit(life, bombs)

  // lettuce will build your strength and extra ruffage
  // will fly in the face of your opponent!!
  def % = Weapon((r, e) => {
      val lettuce = rand(charisma)
      println("[Healthy lettuce gives you "+lettuce+" life points!!]")
      round(r.setLife(r.life + lettuce), e, 0)
    })

  // little boomerang
  def ^ = Weapon((r, e) => round(r, e, 13 ))

  // the hero's sword is unlimited!!
  def / = Weapon((r, e) =>
    round(r, e, rand(4 + ((e.life % 10) * (e.life % 10)))))

  // bombs, but you only have three!!
  def * = Weapon((r, e) =>
    r.bombs match {
      case 0 => println("[UHN!! You're out of bombs!!]")
        round(r, e, 0)
      case n =>
        round(Rabbit(r.life, r.bombs - 1), e, 86)
    })
}

object Rabbit extends Rabbit(10, 3)

trait Monster extends Creature[Monster] {
  def name = "\\w*\\$$".r.findFirstIn(this.getClass.getName).
  flatMap("\\w*".r.findFirstIn) getOrElse "??"

  def setLife(newLife: Int): Monster = new DupMonster(this, newLife)

  override def toString = name+"("+life+")"

  private class DupMonster(old: Monster, val life: Int) extends Monster {
    val strength: Int = old.strength
    val charisma: Int = old.charisma
    val weapon: Int = old.weapon
    override val name: String = old.name
  }
}

trait Tail { this: Monster =>
  def ---< = Weapon((me, it) => round(me, it, rand(me.strength + me.life) +
                                      me.weapon))
}

trait Axe { this: Monster =>
  def |^ = Weapon((me, it) => round(me, it, me.weapon + 45))
}

object ScubaArgentine extends Monster with Axe {
  def life = 46
  def strength = 35
  def charisma = 91
  def weapon = 2
}

object IndustrialRaverMonkey extends Monster {
  def life = 46
  def strength = 35
  def charisma = 91
  def weapon = 2
}

object DwarvenAngel extends Monster with Axe {
  def life = 540
  def strength = 6
  def charisma = 144
  def weapon = 50
}

object AssistantViceTentacleAndOmbudsman extends Monster {
  def life = 320
  def strength = 6
  def charisma = 144
  def weapon = 50
}

object TeethDeer extends Monster {
  def life = 655
  def strength = 192
  def charisma = 19
  def weapon = 109
}

object IntrepidDecomposedCyclist extends Monster {
  def life = 901
  def strength = 560
  def charisma = 422
  def weapon = 105
}

object Dragon extends Monster with Tail {
  def life = 1340
  def strength = 451
  def charisma = 1020
  def weapon = 939
}

object Dwemthy {
  object s {
    val stairs = List(ScubaArgentine, IndustrialRaverMonkey,
                      DwarvenAngel,
                      AssistantViceTentacleAndOmbudsman,
                      TeethDeer,
                      IntrepidDecomposedCyclist,
                      Dragon)
  }
}
