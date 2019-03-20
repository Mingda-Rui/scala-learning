package mingdascala

import scala.annotation.tailrec

sealed trait Slist {
  def isEmpty: Boolean
  def head: Int
  def tail: Slist
  def ::(elem: Int): Slist = new Scons(elem, this)
  def last: Int
  def length: Int

  def penultimate: Int // P02
  def nth(index: Int): Int // P03

  // P05
  def reverse(): Slist = {
    @tailrec def _reverse(accum: Slist, rem: Slist): Slist = rem match {
      case Scons(h, t) => _reverse(Scons(h, accum), t)
      case Snil        => accum
    }
    _reverse(Snil, this)
  }

  // P06
  def isPalindrome(): Boolean = this == this.reverse

  // *P08
  def compress(): Slist = {
    def compressHelper(elem: Int, l: Slist): Slist = l match {
      case Scons(h, t) if h == elem => compressHelper(h, t)
      case Scons(h, t) => h :: compressHelper(h, t)
      case Snil => Snil
    }
    compressHelper(-1, this)
  }

  // *P14
  def duplicate(): Slist = {
    def duplicateHelper(l: Slist): Slist = l match {
      case Scons(h, t) => Scons(h, h::duplicateHelper(t))
      case Snil => Snil
    }
    duplicateHelper(this)
  }

  // *P15
  def duplicateN(times: Int): Slist = {

    def duplicateNHelper(l: Slist): Slist = l match {
      case Scons(h, t) => multiplyN(times, h, duplicateNHelper(t))
      case Snil => Snil
    }

    def multiplyN(times: Int, h: Int, t: Slist): Slist = times match {
      case 0 => t
      case times if times > 0 => Scons(h, multiplyN(times-1, h, t))
    }

    duplicateNHelper(this)
  }

  // *P16
  def drop(nth: Int): Slist = {
    def dropHelper(position: Int, l: Slist): Slist = l match {
      case Scons(h, t) if position % nth == 0 => dropHelper(position+1, t)
      case Scons(h, t) => Scons(h, dropHelper(position+1, t))
      case Snil => Snil
    }
    dropHelper(1, this)
  }

  // *P17
  def split(index: Int): (Slist, Slist) = {
    def splitHelper(i: Int, l: Slist): Slist = l match {
      case Scons(h, t) if i > 1 => Scons(h, splitHelper(i-1, t))
      case Scons(h, t) if i == 1 => Scons(h, Snil)
      case Snil => Snil
    }
    def splitTail(i: Int, l: Slist): Slist = l match {
      case Scons(h, t) if i > 1 => splitTail(i-1, t)
      case Scons(h, t) if i == 1 => t
      case Snil => Snil
    }
    (splitHelper(index, this), splitTail(index, this))
  }

  // *P18
  def slice(i: Int, k: Int): Slist = {
    def sliceHelper(i: Int, k: Int, l: Slist): Slist = l match {
      case Scons(h, t) if i > 0 => sliceHelper(i-1, k-1, t)
      case Scons(h, t) if i <= 0 && k > 0 => Scons(h, sliceHelper(i-1, k-1, t))
      case Scons(h, t) if k == 0 => Snil
      case Snil => Snil
    }
    sliceHelper(i, k, this)
  }

  // *P19
//    def rotate(n: Int): Slist = {
//      def toLeft(i: Int, l: Slist): Slist = l match {
//        case Scons(h, t) if i > 0 => reverseLeft(0, Scons(h, ))
//        case Scons(h, t) if i == 0 => reverseLeft()
//      }
//      def reverseLeft(j: Int, rl: Slist): Slisct = rl match {
//        case Scons(h, t) if j > 0 => Scons(h, reverseLeft(j-1, t))
//        case Scons(h, t) if j == 0 =>
//      }
//      def toRight(i: Int, l: Slist): Slist = l match {
//        case
//      }
//      def reversedList: Slist = this.reverse()
//      // 1 2 3 4 5 -> 3 4 5 1 2
//      // 5 4 3 2 1 -> 2 1 5 4 3
//
//      n match {
//        case n if n > 0 => reverseLeft(this.length-n, reversedList)
//        case n if n < 0 => toRight
//        case n if n == 0 => this
//      }
//    }

  // *P20
  def removeAt(i: Int): Slist =  {
    def removeAtHelper(i: Int, l: Slist): Slist = l match {
      case Scons(h, t) if i > 0 => Scons(h, removeAtHelper(i-1, t))
      case Scons(h, t) if i == 0 => t
      case Scons(h, t) if i < 0 => throw new IllegalArgumentException("negative index is not allowed")
      case Snil => Snil
    }
    removeAtHelper(i, this)
  }

  // *P21
  def insertAt(i: Int, elem: Int): Slist = {
    def insertAtHelper(i: Int, l: Slist): Slist = l match {
      case Scons(h, t) if i > 0 => Scons(h, insertAtHelper(i-1, t))
      case Scons(h, t) if i == 0 => Scons(elem, l)
      case Scons(h, t) if i < 0 => throw new IllegalArgumentException("negative index is not allowed")
      case Snil => Scons(elem, Snil)
    }
    insertAtHelper(i, this)
  }

  // *P22
  def range(start: Int, end: Int): Slist = start match {
    case start if start < end => Scons(start, range(start+1, end))
    case start if start == end => Scons(end, Snil)
    case start if start > end => throw new IllegalArgumentException("start number greater than end number")
  }

  def map(f: Int => Int): Slist = this match {
    case Scons(head, tail) => Scons(f(head), tail.map(f))
    case Snil => Snil
  }

  def foreach(f: Int => Unit): Unit = this match {
    case Scons(head, tail) =>
      f(head)
      tail.foreach(f)
    case Snil => Snil
  }

  def withFilter(f: Int => Boolean): Slist = this match{
    case Scons(head, tail) if f(head) =>
      Scons(head, tail.withFilter(f))
    case Scons(_, t) =>
      t.withFilter(f)
    case Snil => Snil
  }

  def partition(f: Int => Boolean): (Slist, Slist) = {
    (withFilter(f), withFilter(!f(_)))
  }

  def partitionV2(f:Int => Boolean): (Slist, Slist) = {
    val notF: Int => Boolean = (i) => !f(i)
    (this.withFilter(f), this.withFilter(notF))
  }

  def find(f: Int => Boolean): Option[Int] = this match {
    case Scons(head, tail) if f(head) => Some(head)
    case Scons(head, tail) => tail.find(f)
    case Snil => None
  }

  def takeWhile(f: Int => Boolean): Slist = this match {
    case Scons(head, tail) if f(head) => Scons(head, tail.takeWhile(f))
    case _ => Snil
  }

//  def foldLeft(initialVale: Int) (f: (Int, Int) => Int): Int = {
//    //    Slist(1,2,3).foldLeft(0)((acum, i) => acum +i)
//    //    1) f(initialVale, 1) => secondValue
//    //    2) f(secondValue, 2) => thirdValue
//    //    3) f(thirdValue, 3) => result
//    case Scons(h, Snil) => f(initialVale, h)
//    case Scons(h, t) => t.foldLeft(f(initialVale, h))(f)
//  }
//  def foldLeft(initialVale: Int, f:(Int, Int) => Int): Int = {
//      //    Slist(1,2,3).foldLeft(0)((acum, i) => acum +i)
//      //    1) f(initialVale, 1) => secondValue
//      //    2) f(secondValue, 2) => thirdValue
//      //    3) f(thirdValue, 3) => result
//      case Scons(h, Snil) => f(initialVale, h)
//      case Scons(h, t) => t.foldLeft(f(initialVale, h), f)
//  }
}

final case class Scons(val head: Int, val tail: Slist) extends Slist {

  def isEmpty: Boolean = false

  def last: Int = {
    var temp = tail
    var headTemp = head
    while (!temp.tail.isEmpty) {
      temp = tail
      headTemp = tail.head
    }
    headTemp
  }

  def length: Int = 1 + tail.length

  override def toString: String = {
    @annotation.tailrec
    def helper(acum: String, l: Slist): String = {
      if (l.isEmpty) s"$acum)" else helper(s"$acum, ${l.head}", l.tail)
    }
    helper(s"Slist($head", tail)
  }
  override def penultimate: Int = {
    @tailrec
    def helper(l: Slist, pen: Int): Int =
      if (l.tail.isEmpty) pen else helper(l.tail, l.head)
    helper(tail, head)
  }

  override def nth(index: Int): Int = {
    @tailrec def helper(l: Slist, index: Int): Int =
      if (index == 0) {
        l.head
      } else {
        helper(l.tail, index - 1)
      }
    helper(this, index)
  }
}

object Snil extends Slist {
  def isEmpty: Boolean = true
  def head: Int = throw new NoSuchElementException()
  def tail: Slist = throw new NoSuchElementException()
  def last: Int = throw new NoSuchElementException()
  def length: Int = 0

  override def toString: String = "Snil"
  override def penultimate: Int = throw new NoSuchElementException()
  override def nth(index: Int): Int = throw new NoSuchElementException()
}

object Slist {
  def apply(x: List[Int]): Slist = x match {
    case Nil          => Snil
    case head :: tail => new Scons(head, apply(tail))
  }

  def apply(x: Int*): Slist = apply(x.toList)

}

object Test extends App {
  val test = Slist()

  val result = Slist(1, 2) match {
    case Scons(first, Scons(second, Snil)) => "two element list"
    case _                                 => "something else"
  }

//  println(Slist(1, 2, 3, 4, 5, 6, 7).slice(2, 9))
//  println(Slist(1,2,3,4,5,6,7).foldLeft(0)((acum, i) => acum +i))
}
