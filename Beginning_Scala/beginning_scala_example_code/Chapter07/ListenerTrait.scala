case class ChangeEvent[OnType](on: OnType)

trait Listener[T] {
  this: T with Listener[T] =>

  type ChangeHandler = {def changed(c: ChangeEvent[T with Listener[T]]): Unit}
  private var listeners: List[ChangeHandler] = Nil

  def addListener(c: ChangeHandler) = synchronized {listeners = c :: listeners}

  def removeListener(c: ChangeHandler) = synchronized {listeners -= c}

  protected def updateListeners() = synchronized {
    val ch = ChangeEvent(this)
    listeners.foreach(i => i.changed(ch))
  }
}

class Foo extends Listener[Foo] {
  private var _count = 0
  def count = synchronized{_count}
  def inc = synchronized{
    _count += 1
    updateListeners()
  }
}
