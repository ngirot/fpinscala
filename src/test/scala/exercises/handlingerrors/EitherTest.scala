package exercises.handlingerrors

import org.scalatest.FunSuite

class EitherTest extends FunSuite {

  test("Map should apply a function if the Option is not Right") {
    assert(Right(1).map(_ + 1) == Right(2))
  }

  test("Map on left is the Left") {
    assert(Left("error").map(null) == Left("error"))
  }

  test("FlatMap should apply a function for Right") {
    assert(Right(1).flatMap((a) => Right(a + 1)) == Right(2))
  }

  test("FlatMap should not apply a function for Left") {
    assert(Left("error").flatMap(null) == Left("error"))
  }

  test("OrElse should return itself for a Right") {
    assert(Right(1).orElse(Left(2)) == Right(1))
  }

  test("OrElse should return the parameter for a Left") {
    assert(Left("error").orElse(Right(2)) == Right(2))
  }

  test("Map2 should return a Right, result of the function when a and b are Right") {
    assert(Right(1).map2(Right(2))(_ + _) == Right(3))
  }

  test("Map2 should return the Left for a Left object") {
    assert(Left("error").map2(Right(2))(null) == Left("error"))
  }

  test("Map2 should return the parameter for a Left parameter") {
    assert(Right(2).map2(Left("error"))(null) == Left("error"))
  }

}
