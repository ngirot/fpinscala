package exercises.handlingerrors

object Variance {

  /**
   * Exercise 4.2
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(s: Seq[Double]) = if (s.isEmpty) None else Some(s.sum / s.length)

    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))
  }
}
