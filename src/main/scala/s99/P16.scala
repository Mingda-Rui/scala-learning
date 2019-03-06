object P16 extends App {
  def drop(i: Int, l: List[Symbol]): List[Symbol] = {
    
    def dropHelper(temp: Int, l: List[Symbol]): List[Symbol] = l match {
      case h :: t if temp % i == 0 => dropHelper(temp + 1, t)
      case h :: t => h :: dropHelper(temp+1, t)
      case Nil => Nil
    }

    dropHelper(1, l)
  }

  println(drop(3, List('a, 'b, 'c, 'd, 'e)))
  println(drop(4, List('a, 'd)))
}
