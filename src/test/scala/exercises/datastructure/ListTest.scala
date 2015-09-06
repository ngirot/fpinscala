package exercises.datastructure

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

  test("SumLeft should return 0 for an empty list") {
    assert(List.sumLeft(Nil) == 0)
  }

  test("SumLeft should return the sum of all Int of a list") {
    assert(List.sumLeft(List(4, 5, 9)) == 18)
  }

  test("ProductLeft should return 1 for an empty list") {
    assert(List.productLeft(Nil) == 1)
  }

  test("ProductLeft should return the product of all Double of a list") {
    assert(List.productLeft(List(2, 3, 5)) == 30)
  }

  test("LengthLeft of an empty list should be 0") {
    assert(List.lengthLeft(Nil) == 0)
  }

  test("LengthLeft of a non empty list should be the number of element") {
    assert(List.lengthLeft(List(1, 2, 3)) == 3)
  }

  test("Reverse of Nil is Nil") {
    assert(List.reverse(Nil) == Nil)
  }

  test("Reverse should return a list inthe reverse order") {
    assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("FoldLeftUsingFoldRight should fold a list") {
    assert(List.foldLeftUsingFoldRight(List(6, 2, 3), 1.0)(_ * _) == 36)
  }

  test("FoldLeftUsingFoldRight should fold a return the identity value if list is empty") {
    assert(List.foldLeftUsingFoldRight(Nil: List[Int], 5)(_ + _) == 5)
  }

  test("FoldRightUsingFoldLeft should fold a list") {
    assert(List.foldRightUsingFoldLeft(List(6, 2, 3), 1.0)(_ * _) == 36)
  }

  test("FoldRightUsingFoldLeft should fold a return the identity value if list is empty") {
    assert(List.foldRightUsingFoldLeft(Nil: List[Int], 5)(_ + _) == 5)
  }

  test("Append should create a list with the elements of two others in order") {
    assert(List.append(List(1, 2), List(3, 4)) == List(1, 2, 3, 4))
  }

  test("Append an empty list to another list should return the other list") {
    assert(List.append(List(1, 2), Nil) == List(1, 2))
  }

  test("Append a list to an empty list should return the initial list") {
    assert(List.append(Nil, List(1, 2)) == List(1, 2))
  }

  test("Append an empty list to another empty list should be an emtpy list") {
    assert(List.append(Nil, Nil) == Nil)
  }

  test("Concat an empty list is an empty list") {
    assert(List.concat(Nil) == Nil)
  }

  test("Concat a list a one single list should be the list") {
    assert(List.concat(List(List(1, 2))) == List(1, 2))
  }

  test("Concat a list two different list should one big list with all elements from the two lists") {
    assert(List.concat(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))
  }

  test("addOne on an empty list should return an empty list") {
    assert(List.addOne(Nil) == Nil)
  }

  test("addOne should return a list with all element of an other, plus one") {
    assert(List.addOne(List(1, 5, 9)) == List(2, 6, 10))
  }

  test("DoubleAsString on an empty list should return an empty list") {
    assert(List.doubleToString(List(1.0, 2.5)) == List("1.0", "2.5"))
  }

  test("Map on an empty list should return an empty list") {
    assert(List.map(Nil: List[Int])(_ + 1) == Nil)
  }

  test("Map should apply fonction all on element on a list") {
    assert(List.map(List(1, 5))(_ + 1) == List(2, 6))
  }

  test("Filter should create a list without all element match a function") {
    assert(List.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == List(2, 4, 6))
  }

  test("FlatMap should retun a list from all element created from the fonction") {
    assert(List.flatMap(List(1, 2))(el => List(el, el)) == List(1, 1, 2, 2))
  }

  test("FilterUsingFlatMap should create a list without all element match a function") {
    assert(List.filterUsingFlatMap(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == List(2, 4, 6))
  }

}
