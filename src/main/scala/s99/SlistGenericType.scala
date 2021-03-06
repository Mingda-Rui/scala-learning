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
  
  def map[E](f: T => E): Slist[E] = this match {
    case Snil => Snil
    case Scons(head, tail) => Scons(f(head), tail.map(f))
  }
  
  def foreach(f: T => Unit): Unit = this match {
    case Snil => Snil
    case Scons(head, tail) => f(head); tail.foreach(f)
  }
  
  def withFilter(f: T => Boolean): Slist[T] = this match {
    case Snil => Snil
    case Scons(head, tail) if f(head) => Scons(head, tail.withFilter(f))
    case Scons(head, tail) => tail.withFilter(f)
  }
  
  def foldLeft[E](init: E)(f: (E, T) => E): E = this match {
    case Snil => init
    case Scons(head, tail) => tail.foldLeft(f(init, head))(f)
  }
  
  // List(List(1,2,3),List(4,5,6),List(8,9))
  // i.g. B int, T is a list of Int
  // def flattern[B: T is a Slist[B]]
  def flattern[B](implicit ev: T <:< Slist[B]): Slist[B] = {
    this.foldLeft[Slist[B]](Snil) { (acum, sublist) =>
      sublist.foldLeft[Slist[B]](acum)((acum2, elem) => elem :: acum2)
    }.reverse
  }
  
  def flatMap[B](f: T => Slist[B]): Slist[B] = {
    this.map(f).flattern
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
  
  slist1.foreach((s: String) => print(s + " ")); println
  slist2.foreach((i: Int) => print(i + " ")); println
  
  println(slist1.withFilter(_ != "b"))
  println(slist2.withFilter(_ >= 2))
  
  println(slist1.foldLeft("z")(_ + _))
  println(slist2.foldLeft(0)(_ + _))
  
  val list = for {
    l <- Slist(1, 2, 3, 4) if l % 2 == 0
    l2 <- Slist(5, 6, 7, 8)
  } yield l + l2
  println(list)

  println(Slist(Slist(1,2,3), Slist(4,5,6)).flattern)
  
}
