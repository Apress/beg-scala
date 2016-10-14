trait MyListener {
  def changed(event: Foo, count: Int): Unit
}

class Foo {
  private var listeners: List[MyListener] = Nil
  private var count = 0

  def access() = synchronized {
    notifyListeners
    count += 1
    count
  }

  private def notifyListeners = synchronized {
    listeners.foreach(_.changed(this, count))
  }

  def addListener(who: MyListener): Unit = synchronized {
    listeners ::= who
  }
}

class FooListener(foo: Foo) extends MyListener {
  foo.addListener(this)

  def changed(event: Foo, count: Int): Unit = {
    if (count < 10) event.access()
  }
}
