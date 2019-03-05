object P14 extends App {
  def duplicate(l: List[String]): List[String] = l match {
    case h :: t => h :: h :: duplicate(t)
    case Nil => Nil
  }

  println(duplicate(List("a", "b", "c", "c")))
}
