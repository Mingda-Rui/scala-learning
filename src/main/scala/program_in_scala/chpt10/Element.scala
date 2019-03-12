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

class LineElement(s: String) extends ArrayElement(Array(s)) {
  override val width = s.length
  override val height = 1
  override def demo() = {
    println("LineElement's implementation invoked")
  }
}

class UniformElement(
  ch: Char,
  override val width: Int,
  override val height: Int,
) extends Element {
  private val line = ch.toString * width
  def contents = Array.fill(height)(line)
}