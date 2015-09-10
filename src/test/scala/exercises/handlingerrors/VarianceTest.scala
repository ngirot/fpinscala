package exercises.handlingerrors

import org.scalatest.FunSuite

class VarianceTest extends FunSuite {

  test("Variance of a list of double") {
    val s: Seq[Double] = List(1.0, 2.0, 5.0, 100.0);

    assert(Variance.variance(s) == Some(1778.5))
  }

  test("The is no variance for empty list") {
    assert(Variance.variance(Nil) == None)
  }
}
