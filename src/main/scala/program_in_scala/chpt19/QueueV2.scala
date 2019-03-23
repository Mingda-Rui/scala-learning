trait QueueV2[T] {
  def head: T
  def tail: QueueV2[T]
  def enqueue(x: T): QueueV2[T]
}

object QueueV2 {
  def apply[T](xs: T*): QueueV2[T] = 
    new QueueV2Impl[T](xs.toList, Nil)

  private class QueueV2Impl[T](
    private val leading: List[T],
    private val trailing: List[T]    
  ) extends QueueV2[T] {
    def mirror = 
      if (leading.isEmpty)
        new QueueV2Impl(trailing.reverse, Nil)
      else 
        this
    def head: T = mirror.leading.head
    def tail: QueueV2Impl[T] = {
      val q = mirror
      new QueueV2Impl(q.leading.tail, q.trailing)
    }
    def enqueue(x: T) =
      new QueueV2Impl(leading, x :: trailing)
  }
}