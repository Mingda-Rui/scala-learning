object P04 {
    def length(l: List[Int]) : Int = l match {
        case h :: Nil  => 1
        case _ :: tail => length(tail) + 1
        case Nil => 0
    }
}