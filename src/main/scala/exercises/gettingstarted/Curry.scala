package exercises.gettingstarted

object Curry {

  /**
   * Exercise 2.3
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  /**
   * Exercise 2.4
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

}
