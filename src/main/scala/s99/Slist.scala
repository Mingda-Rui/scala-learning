package scalaweekly

import scala.annotation.tailrec

sealed trait Slist {
  def isEmpty: Boolean
  def head: Int
  def tail: Slist
  def ::(elem: Int): Slist = new Scons(elem, this)
  def last: Int
  def length: Int

  def penultimate: Int // P02
  def nth(index: Int): Int //P04
  def reverse(): Slist = { //P05
    @tailrec def _reverse(accum: Slist, rem: Slist): Slist = rem match {
      case Scons(h, t) => _reverse(Scons(h, accum), t)
      case Snil        => accum
    }
    _reverse(Snil, this)
  }
  def isPalindrome(): Boolean = this == this.reverse //P06
  def compress(): Slist = { //P08
    def _compress(elem: Int, l: Slist): Slist = l match {
      case Scons(h, tail) if elem == h => _compress(elem, tail)
      case Scons(h, tail)              => Scons(h, _compress(h, tail))
      case Snil                        => Snil
    }
    _compress(-1, this)
  }

  //def duplicate(): Slist //P14
  def duplicate(): Slist = {
    def _duplicate(l: Slist): Slist = l match {
      case Scons(h, tail) => Scons(h, Scons(h, _duplicate(tail)))
      case Snil => Snil
    }
    _duplicate(this)
  }
  def duplicateN(times: Int): Slist = { //P15
    @tailrec def helper(timesLeft: Int, acum: Slist, rem: Slist): Slist = {
      if(!rem.isEmpty) {
        if(timesLeft > 0) {
          helper(timesLeft - 1, Scons(rem.head, acum), rem)
        } else {
          helper(times, acum, rem.tail)
        }
      } else {
        acum.reverse()
      }
    }
    helper(times, Snil, this)
  }
  //def drop(nth: Int): Slist //P16
  //def split(index: Int): (Slist, Slist) //P17
  //def slice(i: Int, k: Int): Slist //P18
  //def removeAt(i: Int): Slist //P20
  //def insertAt(i: Int, elem: Int): Slist //P21
  //def range(start: Int, end: Int): Slist //P22
  //def rotate(n: Int): Slist //P19

  // finish later
  def map(f: Int => Int): Slist = this match {
    case Scons(head, tail) => Scons(f(head), tail.map(f))
    case Snil => Snil
  }
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

  println(Slist(1, 2, 3).duplicate())
}
