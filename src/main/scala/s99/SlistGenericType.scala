package mingda.scalalearning.generictype


sealed trait Slist[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: Slist[T]
  def ::[E >: T](elem: E): Slist[E] = new Scons(elem, this)
  def length: Int

  // Homework:
  // Read thoroughly the new implementation of Slist and then add the methods
  // last, reverse, map, foreach, withFilter and foldLeft
  // (obviously you can copy as much as you can from the old Slist)
  // We will finally implement in class flattern and flatMap

  def last: T = this match {
    case Scons(head, Snil) => head
    case Scons(head, tail) => tail.last
    case Snil => throw new IllegalArgumentException("Empty Slist")
  }
  
  def reverse: Slist[T] = {
    def reverseHelper(current: Slist[T], result: Slist[T]): Slist[T] = current match {
      case Snil => result
      case Scons(head, tail) => reverseHelper(tail, Scons(head, result))
    }
    reverseHelper(this, Snil)
  }
  
  def map[E >: T](f: E => E): Slist[E] = this match {
    case Snil => Snil
    case Scons(head, tail) => Scons(f(head), tail.map(f))
  }
  
}

final case class Scons[T](val head: T, val tail: Slist[T]) extends Slist[T] {

  def isEmpty: Boolean = false

  def length: Int = 1 + tail.length

  override def toString: String = {
    @annotation.tailrec
    def helper(acum: String, l: Slist[T]): String = {
      if (l.isEmpty) s"$acum)" else helper(s"$acum, ${l.head}", l.tail)
    }
    helper(s"Slist($head", tail)
  }
}

object Snil extends Slist[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException()
  def tail: Slist[Nothing] = throw new NoSuchElementException()
  def length: Int = 0

  override def toString: String = "Snil"
}

object Slist {
  def apply[T](x: List[T]): Slist[T] = x match {
    case Nil          => Snil
    case head :: tail => new Scons[T](head, apply(tail))
  }

  def apply[T](x: T*): Slist[T] = apply(x.toList)

}

object Test extends App {
  val slist1 = Slist("a", "b", "c")
  val slist2 = Slist(1, 2, 3)
  println(1 :: 2 :: 3 :: Snil)

  println(slist1.last)
  println(slist2.last)
  
  println(slist1.reverse)
  println(slist2.reverse)
  
  println(slist1.map((s: String) => s + "XL"))
  println(slist2.map((i: Int) => i + 1))
  
}
