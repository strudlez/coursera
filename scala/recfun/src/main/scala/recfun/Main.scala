package recfun
import common._

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
    def factorial(n: Int, acc: Long): Long = if (n <= 0) acc else factorial(n - 1, n * acc)
    (factorial(r, 1) / (factorial(c, 1) * factorial(r - c, 1))).toInt
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_(open: Int, chars: List[Char]): Boolean = chars match {
      case '(' :: xs => balance_(open + 1, xs)
      case ')' :: xs => if (open > 0) balance_(open - 1, xs) else false
      case _ :: xs   => balance_(open, xs)
      case Nil       => open == 0
    }

    balance_(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def combinationsThatEqual(target: Int, values: List[Int]): Int = values match {
      case Nil     => 0
      case x :: xs => {
        if (target == x) {
          1
        } else if (target < x) {
          0
        } else {
          combinationsThatEqual(target - x, values) + combinationsThatEqual(target, xs)
        }
      }
    }

    combinationsThatEqual(money, coins.sortWith(_ < _))
  }
}
