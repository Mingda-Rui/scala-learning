package mingda.scala.learning.chpt10

import Element.elem

abstract class Element {
  def contents: Array[String]
  val height: Int = contents.length
  val width: Int = 
    if (height == 0) 0 else contents(0).length
  
  def demo() = {
    println("Element's implementation invoked")
  }

  def above(that: Element): Element = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    elem(this1.contents ++ that1.contents)
  }
    

  def beside(that: Element): Element =
    elem(
      for (
        (line1, line2) <- this.contents zip that.contents
      ) yield line1 + line2
    )

  def widen(w: Int): Element =
    if (w <= width) this
    else {
      val left = elem(' ', (w -width) / 2, height)
      val right = elem(' ', w - width - left.width, height)
      left beside this beside right
    }

  override def toString = contents mkString "\n"
} 

object Element {

  // Original ArrayElement
  // class ArrayElement(conts: Array[String]) extends Element {
  //    def contents: Array[String] = conts
  // }

  // parametric field definition
  private class ArrayElement (
    val contents: Array[String]
  ) extends Element {
    override def demo() = {
      println("ArrayELement's implementation invoked")
    }
  }

  private class LineElement(s: String) extends Element {
    val contents = Array(s)
    override val width = s.length
    override val height = 1
  }

  private class UniformElement(
    ch: Char,
    override val width: Int,
    override val height: Int,
  ) extends Element {
    private val line = ch.toString * width
    def contents = Array.fill(height)(line)
  }

  def elem(contents: Array[String]): Element =
    new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)

  def elem(line: String): Element = 
    new LineElement(line)
}