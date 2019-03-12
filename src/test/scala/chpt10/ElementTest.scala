import mingda.scala.learning.chpt10.Element
import mingda.scala.learning.chpt10.ArrayElement

object ElementTest {
    def main(args: Array[String]): Unit = {
        val e: Element = new ArrayElement(Array("hello"))
        for(s <- e.contents) {
            println(s)
        }
    }
}