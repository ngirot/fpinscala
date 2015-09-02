package exercices.datastructure

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  test("Tail should remove first item of a list") {
    assert(List.tail(List(1, 2, 3)) == List(2, 3))
  }

  test("Tail should return Nil if the list is Nil") {
    assert(List.tail(Nil) == Nil)
  }

  test("Tail should return Nil for a list with only one element") {
    assert(List.tail(List(1)) == Nil)
  }

  test("SetHead should return a list with a different first element") {
    assert(List.setHead(List(1, 2, 3), 0) == List(0, 2, 3))
  }

  test("SetHead fail if the list is empty") {
    intercept[IllegalArgumentException] {
      List.setHead(Nil, 0)
    }
  }

  test("SetHead should return a new list with ontly the element for a list with only one element") {
    assert(List.setHead(List(1), 0) == List(0))
  }

}
