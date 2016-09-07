package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == r || c == 0) {
        1
      } else {
        pascal(c-1, r-1) + pascal(c, r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def helper(lefts: Int, remains: List[Char]) : Boolean = {
        if (remains.isEmpty) {
          lefts == 0
        } else if (remains.head == '(') {
          helper(lefts + 1, remains.tail)
        } else if (remains.head == ')') {
          if (lefts == 0) false else helper(lefts - 1, remains.tail)
        } else {
          helper(lefts, remains.tail)
        }
      }
      helper(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0) {
        0
      } else if (money == 0) {
        1
      } else if (coins.isEmpty) {
        0
      } else {
        countChange(money - coins.head, coins) + 
        countChange(money, coins.tail)
      }
    }
  }
