object test {
  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3)
    val r = l match {
      case head :: Nil => println("2")
      case head :: last => println("last 2")
      case head :: tail => println("1") 
      case Nil => println("empty")
    }
  }
}

