package recfun

import scala.collection.mutable.ListBuffer

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    print("Parentheses Balancing")
    balance("(if (zero? x) max (/ 1 x))".toList)
    balance("I told him (that it’s not (yet) done). (But he wasn't listening)".toList)
    balance(":-)".toList)
    balance("())(".toList)
  }

  /**
   * Exercise 1: Pascal's Triangle
   * The numbers at the edge of the triangle are all 1,
   * and each number inside the triangle is the
   * sum of the two numbers above it
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2: Parentheses Balancing
   */
  def balance(chars: List[Char]): Boolean = {
    def f(chars: List[Char], numOpens: Int): Boolean = {
      if (chars.isEmpty) {
        numOpens == 0
      } else {
        val h = chars.head
        val n =
          if (h == '(') numOpens + 1
          else if (h == ')') numOpens - 1
          else numOpens
        if (n >= 0) f(chars.tail, n)
        else false
      }
    }

    f(chars, 0)
  }

  /**
   * Exercise 3: Counting Change
   * Write a recursive function that counts how many different
   * ways you can make change for an amount, given a list of coin denominations.
   * For example, there are 3 ways to give change for 4 if you have coins with denomiation
   * 1 and 2: 1+1+1+1, 1+1+2, 2+2.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
