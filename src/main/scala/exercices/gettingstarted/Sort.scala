package exercices.gettingstarted

import scala.annotation.tailrec

object Sort {

  /**
   * Exercice 2.2
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def go(n: Int): Boolean = {
      if (n <= 0) true
      else if (ordered(as(n - 1), as(n))) go(n - 1)
      else false
    }

    go(as.length - 1)
  }
}
