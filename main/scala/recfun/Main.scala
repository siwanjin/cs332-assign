package recfun
import common._

object Main {
  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
      
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], openCount: Int): Boolean = {
      if (chars.isEmpty)
        openCount == 0
      else if (openCount < 0)
        false
      else chars.head match {
        case '(' => loop(chars.tail, openCount + 1)
        case ')' => loop(chars.tail, openCount - 1)
        case _   => loop(chars.tail, openCount)
      }
    }
    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if(money < 0 || coins.isEmpty)
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
      
}
