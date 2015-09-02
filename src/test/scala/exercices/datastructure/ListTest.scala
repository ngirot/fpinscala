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

}
