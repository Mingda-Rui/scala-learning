object P05 {
    def reverse(l: List[Int]) : List[Int] = l match {
        case h :: Nil  => List(h)
        case h :: tail => reverse(tail) ::: List(h)
        case _         => throw new NoSuchElementException
    }
}