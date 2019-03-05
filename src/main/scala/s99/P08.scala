object P08 extends App{
  def compress(l: List[String]): List[String] = {

    //    def compressHelper(l: List[String], element: String): List[String] = {
    //      var e = element
    //      if (l.head != element) {
    //        e = l.head
    //        e :: compressHelper(l.tail, e)
    //      } else {
    //        compressHelper(l.tail, e)
    //      }
    //    }

    def compressHelperV2(l: List[String], e: String): List[String] = l match {
      case head :: tail if head == e => compressHelperV2(tail, e)
      case head :: tail => head :: compressHelperV2(tail, head)
      case Nil => Nil
    }

    def result = compressHelperV2(l, "")
    result
  }

  println(compress(List("a", "a", "a", "b", "c", "c")))
}