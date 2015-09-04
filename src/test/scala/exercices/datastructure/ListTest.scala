package exercices.datastructure

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  val even = (i: Int) => i % 2 == 0

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

  test("DropWhile should remove only the all firsts element matching the condition") {
    assert(List.dropWhile(List(2, 4, 5, 6), even) == List(5, 6))
  }

  test("DropWhile return an empty list if the list is empty") {
    assert(List.dropWhile(Nil, even) == Nil)
  }

  test("DropWhile return the entire list if no element matches the condition") {
    assert(List.dropWhile(List(1, 3, 5), even) == List(1, 3, 5))
  }

  test("DropWhile return an empty list if every element matches the condition") {
    assert(List.dropWhile(List(2, 4, 6), even) == Nil)
  }

  test("Init should return the complete list without the last element") {
    assert(List.init(List(1, 2, 3)) == List(1, 2))
  }

  test("Init should fail when we try to remove last element of an empty list") {
    intercept[IllegalArgumentException] {
      List.init(Nil)
    }
  }

  test("Init should return Nil for a list with only one element") {
    assert(List.init(List(1)) == Nil)
  }

  test("Length of an empty list should be 0") {
    assert(List.length(Nil) == 0)
  }

  test("Length of a non empty list should be the number of element") {
    assert(List.length(List(1, 2, 3)) == 3)
  }

  test("FoldLeft should fold a list") {
    assert(List.foldLeft(List(6, 2, 3), 1.0)(_ * _) == 36)
  }

  test("FoldLeft should fold a return the identity value if list is empty") {
    assert(List.foldLeft(Nil: List[Int], 5)(_ + _) == 5)
  }

}
