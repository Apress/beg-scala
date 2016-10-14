object Control {
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B = 
  try {
    f(param)
  } finally {
    param.close()
  } 

  import scala.collection.mutable.ListBuffer

  def bmap[T](test: => Boolean)(block: => T): List[T] = {
    val ret = new ListBuffer[T]
    while(test) ret += block
    ret.toList
  }
}
