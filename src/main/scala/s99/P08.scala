object P08 extends App{
  def compress(l: List[String]): List[String] = {

    def compressHelper(l: List[String], r: List[String]): List[String] = {

      if (r.head != l.tail.head) {
        newList.head :: r
      }
      compressHelper(newList.tail, r)

      r
    }

    def result = compressHelper(l.tail, List(l.head))
    result.reverse
  }

  println(compress(List("a", "a", "a", "b", "c", "c")))
}
