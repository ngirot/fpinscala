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

  test("Sequence should convert a list of Either into a Right of a list if all Either are Right") {
    assert(Either.sequence(List(Right(1), Right(2))) == Right(List(1, 2)))
  }

  test("Sequence should convert a list of Either into a the first Left of the sequence if one of the element is left") {
    assert(Either.sequence(List(Right(1), Left("error"), Left("another error"))) == Left("error"))
  }

  test("Traverse should apply a function to all element and return list in an Right if all of them are not Left") {
    assert(Either.traverse(List(1, 2, 3))(i => Right(i * 2)) == Right(List(2, 4, 6)))
  }

  test("Traverse should return the first Left if any element where the function is applied equals Left") {
    assert(Either.traverse(List(1, 2, 3))(i => if (i == 2) Left("error" + i) else Right(i)) == Left("error2"))
  }

}
