object P02 {
  /*  def penultimate(l: Slist): Int = l match {
      case Scons(head, tail) => penultimate(tail)
      case head => head
      case Snil => throw new IllegalArgumentException("List is empty")
    }*/
  def penultimate(l: List[Int]): Int = l match {
    case head :: last :: Nil => head
    case head :: tail => penultimate(tail)
    case Nil => throw new IllegalArgumentException("Empty list")
    case head :: Nil => throw new IllegalArgumentException("one element")
  }

  def main(args: Array[String]): Unit = {
    println(penultimate(List(1,2,3)))
  }

}