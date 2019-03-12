import mingda.scala.learning.chpt10.Element
import mingda.scala.learning.chpt10.ArrayElement
import mingda.scala.learning.chpt10.LineElement
import mingda.scala.learning.chpt10.UniformElement

object ElementTest {
    def main(args: Array[String]): Unit = {
        val e: Element = new ArrayElement(Array("hello"))
        for(s <- e.contents) {
            println(s)
        }

        // Polymorphism
        invokeDemo(new ArrayElement(Array("a", "b")))
        invokeDemo(new LineElement("c"))
        invokeDemo(new UniformElement('d', 1, 2))
    }

    def invokeDemo(e: Element) = {
        e.demo()
    } 
}