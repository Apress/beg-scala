abstract class LivingThing
abstract class Plant extends LivingThing
abstract class Fungus extends LivingThing
abstract class Animal extends LivingThing

trait HasLegs extends Animal {
  def walk() {println("Walking")}
}

trait HasWings extends Animal {
  def flap() {println("Flap Flap")}
}

trait Flies {
  this: HasWings =>
  def fly() {println("I'm flying")}
}

abstract class Bird extends Animal with HasWings with HasLegs

class Robin extends Bird with Flies
class Ostrich extends Bird

abstract class Mammal extends Animal {
  def bodyTemperature: Double
}

trait KnowsName extends Animal {
  def name: String
}

class Dog(val name: String) extends Mammal with HasLegs with KnowsName {
  def bodyTemperature: Double = 99.3
}

trait IgnoresName {
  this: KnowsName =>
  def ignoreName(when: String): Boolean

  def currentName(when: String): Option[String] =
    if (ignoreName(when)) None else Some(name)
}

class Cat(val name: String) extends Mammal with HasLegs with
  KnowsName with IgnoresName {
  def ignoreName(when: String) = when match {
    case "Dinner" => false
    case _ => true
  }
  def bodyTemperature: Double = 99.5
}

trait Athlete extends Animal

trait Runner {
  this: Athlete with HasLegs =>
  def run() {println("I'm running")}
}

class Person(val name: String) extends Mammal with
  HasLegs with KnowsName {
  def bodyTemperature: Double = 98.6
}

trait Biker extends Person {
  this: Athlete=>
  def ride() {println("I'm riding my bike")}
}

trait Gender
trait Male extends Gender
trait Female extends Gender
