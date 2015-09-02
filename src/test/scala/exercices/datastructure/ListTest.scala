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

  test("Drop should remove the nth elements of a list with more than n element") {
    assert(List.drop(List(1, 2, 3), 2) == List(3))
  }

  test("Drop should fail when we try to remove more element that are in the list") {
    intercept[IllegalArgumentException] {
      List.drop(List(1, 2), 3)
    }
  }

  test("Drop should fail when we try to remove a negative number of element from the list") {
    intercept[IllegalArgumentException] {
      List.drop(List(1, 2), -1)
    }
  }

  test("Drop should return the exact same list when n is equal to zero") {
    assert(List.drop(List(1, 2, 3), 0) == List(1, 2, 3))
  }

}
