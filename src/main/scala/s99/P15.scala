object P15 extends App {
  def duplicateN(i: Int, l: List[Symbol]): List[Symbol] = {
    def helper(i: Int, s: Symbol): List[Symbol] = i match {
      case i if i > 0 => s :: helper(i-1, s)
      case 0 => Nil
    }
    l match {
      case h :: t => helper(i, h) ::: duplicateN(i, t)
      case Nil => Nil
    }
  }

  println(duplicateN(3, List('c, 'b, 'a)))
}
