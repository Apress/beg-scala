trait SafeMap {
  import scala.reflect._
  class SafeCast(in: Any) {
    def is[T](implicit man: Manifest[T]): Option[T] = {
      // special case for Boolean
      val cls = if (man.toString == "boolean") classOf[java.lang.Boolean]
      else man.erasure

      in match {
        case null => None
        case t: AnyRef if cls.isAssignableFrom(t.getClass) =>
          Some(t.asInstanceOf[T])
        case _ => None
      }
    }
  }

  class SafeMapC[A, B](m: Map[A, B]) {
    def sGet[T](key: A)(implicit man: Manifest[T]): Option[T] =
    m.get(key).flatMap(v => new SafeCast(v).is(man))
  }
  implicit def mToSM[A, B](in: Map[A, B]) = new SafeMapC(in)
  implicit def iToIs(v: Any) = new SafeCast(v)
}
