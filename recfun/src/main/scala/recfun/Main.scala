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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == c)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balanceIter(chars: List[Char], openParenthesisNotclosed: Int): Boolean = {
      if (openParenthesisNotclosed < 0)
        false
      else if (chars.isEmpty)
        openParenthesisNotclosed == 0
      else balanceIter(
        chars.tail,
        if (chars.head == '(') openParenthesisNotclosed + 1
        else if (chars.head == ')') openParenthesisNotclosed - 1
        else openParenthesisNotclosed
      )
    }
    balanceIter(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeIter(payback: Int, coinsLeft: List[Int]) : Int = {
      if (payback == money)
        1
      else if (coinsLeft.isEmpty || payback > money) 0
      else
        countChangeIter(payback, coinsLeft.tail) +
        countChangeIter(payback + coinsLeft.head, coinsLeft)
    }
    countChangeIter(0, coins)
  }
}
