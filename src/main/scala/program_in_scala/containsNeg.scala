object containsNeg {

    // Determine whether a passed List contains a negative number:
    def containsNeg(nums: List[Int]): Boolean = {
        var exists = false
        for (num <- nums)
          if (num < 0)
            exists = true
        exists
    }

    // Use higher order function
    def containsNegV2(nums: List[Int]) = nums.exists(_ < 0)

    def main(args: Array[String]): Unit = {
        println(containsNeg(List(1,2,3,4)))
        println(containsNegV2(List(1,2,-3,4)))
    }
}