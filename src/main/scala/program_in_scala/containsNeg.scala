object containsNeg {

    // Determine whether a passed List contains a negative number:
    def containsNeg(nums: List[Int]): Boolean = {
        var exists = false
        for (num <- nums)
          if (num < 0)
            exists = true
        exists
    }

    def main(args: Array[String]): Unit = {
        println(containsNeg(List(1,2,3,4)))
    }
}