class Queue[+T] private (
  private[this] val leading: List[T],
  private[this] val trailing: List[T]
) {
  private def mirror = 
    if (leading.isEmpty) {
      while (!trailing.isEmpty) {
        leading = trailing.head :: leading
        trailing = trailing.tail  
      }  
    }

  def head: T = {
    mirror()
    leading.head  
  }
  def tail: Queue[T] = {
    mirror()
    new Queue(leading.tail, trailing)
  }
  def enqueue[U >: T](x: U) = 
    new Queue[U](leading, x :: trailing)
}  

object Queue {
  // constructs a queue with initial elements 'xs'
  def apply[T](xs: T*) = new Queue[T](xs.toList, Nil)
}