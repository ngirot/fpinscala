package exercises.handlingerrors

import org.scalatest.FunSuite

class OptionTest extends FunSuite {

  test("Map should apply a function if the Option is not None") {
    assert(Some(1).map(_ + 1) == Some(2))
  }

  test("Map on None should be None") {
    assert(None.map(null) == None)
  }

  test("GetOrElse should return the value of Some") {
    assert(Some(1).getOrElse(2) == 1)
  }

  test("GetOrElse should the default value for None") {
    assert(None.getOrElse(2) == 2)
  }

  test("FlatMap should apply a function if the Option is not None") {
    assert(Some(1).flatMap((a) => Some(a + 1)) == Some(2))
  }

  test("FlatMap on None should be None") {
    assert(None.flatMap(null) == None)
  }

  test("Filter should return the the Option if the condition is true") {
    assert(Some(2).filter(_ % 2 == 0) == Some(2))
  }

  test("Filter should return None if the condition is false") {
    assert(Some(3).filter(_ % 2 == 0) == None)
  }

  test("Filter should return None if the Option is None") {
    assert(None.filter(null) == None)
  }

  test("OrElse should return the Option if defined") {
    assert(Some(1).orElse(Some(2)) == Some(1))
  }

  test("OrElse should return the parameter if Option is not defined") {
    assert(None.orElse(Some(2)) == Some(2))
  }

  test("Map2 should return an element, result of the function when a and b and Some") {
    assert(Option.map2(Some(1), Some(2))(_ + _) == Some(3))
  }

  test("Map2 should return None if a is None") {
    assert(Option.map2(None: Option[Int], Some(2))(_ + _) == None)
  }

  test("Map2 should return None if b is None") {
    assert(Option.map2(Some(1), None: Option[Int])(_ + _) == None)
  }

  test("Sequence should convert a list of option into a option of a list if all option are not None") {
    assert(Option.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  }

  test("Sequence shoudl convert a list of option into None if one of the element is None") {
    assert(Option.sequence(List(Some(1), None, Some(3))) == None)
  }
}
