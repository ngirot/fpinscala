package exercices.gettingstarted

class Compose {

  /**
   * Exercice 2.5
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
