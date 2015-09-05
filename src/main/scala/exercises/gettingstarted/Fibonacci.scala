package exercises.gettingstarted

import scala.annotation.tailrec

object Fibonacci {

  /**
   * Exercise 2.1
   */
  def fib(n: Int): Int = {

    @tailrec
    def go(n: Int, previous: Int, current: Int): Int =
      if (n == 0) previous
      else go(n - 1, current, previous + current)

    go(n, 0, 1)
  }

}
