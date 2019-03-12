package mingda.scala.learning.chpt10

abstract class Element {
  def contents: Array[String]
  val height = contents.length
  val width =
    if (height == 0) 0 else contents(0).length    
} 

// Original ArrayElement
// class ArrayElement(conts: Array[String]) extends Element {
//    def contents: Array[String] = conts
// }

// parametric field definition
class ArrayElement (
  val contents: Array[String]
) extends Element
