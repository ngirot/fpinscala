package exercices.gettingstarted

object Fibonacci {

  def fib(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, previous: Int, current: Int): Int =
      if (n == 0) previous
      else go(n - 1, current, previous + current)

    go(n, 0, 1)
  }

}
