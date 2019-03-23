class SlowAppendQueue[T](elems: List[T]) { // Not efficient
  def head = elems.head
  def tail = new SlowAppendQueue(elems.tail)
  def enqueue(x: T) = new SlowAppendQueue(elems ::: List(x))
}

class SlowAppendQueueV2[T](smele: List[T]) { // Not efficient
  // smele is elems reversed 
  def head = smele.last
  def tail = new SlowAppendQueueV2(smele.init)
  def enqueue(x: T) = new SlowAppendQueueV2(x :: smele)
}