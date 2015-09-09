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
}
