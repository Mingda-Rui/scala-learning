package mingda.scala.learning.chpt10

abstract class Element {
  def contents: Array[String]
  val height = contents.length
  val width = {
    try {
      if (height == 0) 0
      else contents(0).length
    } catch {
      case _: Exception => 0
    }
  }

  def demo() = {
    println("Element's implementation invoked")
  }

  def above(that: Element): Element =
    new ArrayElement(this.contents ++ that.contents)

  def beside(that: Element): Element =
    new ArrayElement(
      for (
        (line1, line2) <- this.contents zip that.contents
      ) yield line1 + line2
    )

  override def toString = contents mkString "\n"
} 

// Original ArrayElement
// class ArrayElement(conts: Array[String]) extends Element {
//    def contents: Array[String] = conts
// }

// parametric field definition
class ArrayElement (
  val contents: Array[String]
) extends Element {
  override def demo() = {
    println("ArrayELement's implementation invoked")
  }
}

class LineELement(s: String) extends Element {
  val contents = Array(s)
  override val width = s.length
  override val height = 1
}

class UniformElement(
  ch: Char,
  override val width: Int,
  override val height: Int,
) extends Element {
  private val line = ch.toString * width
  def contents = Array.fill(height)(line)
}

object Element {
  def elem(contents: Array[String]): Element =
    new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)

  def elem(line: String): Element = 
    new LineElement(line)
}