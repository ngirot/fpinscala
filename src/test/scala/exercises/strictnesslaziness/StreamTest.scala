package exercises.strictnesslaziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("ToList should convert a Stream into a List") {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
  }

  test("ToList of an empty Stream is an empty List") {
    assert(Empty.toList == Nil)
  }

  test("Take should return the first elements of a Stream") {
    assert(Stream(1, 2, 3, 4, 5).take(3).toList == Stream(1, 2, 3).toList)
  }

  test("Take should return an empty Steam when we take the 0th first elements") {
    assert(Stream(1, 2).take(0).toList == Nil)
  }

  test("Take should throw an error when n is negative") {
    intercept[IllegalArgumentException] {
      Stream(1, 2).take(-1)
    }
  }

  test("Take should return the entire stream when we take more element than the number of elements in the stream") {
    assert(Stream(1, 2).take(3).toList == Stream(1, 2).toList)
  }

  test("Drop should return a Stream without its nth elements") {
    assert(Stream(1, 2, 3, 4, 5).drop(3).toList == Stream(4, 5).toList)
  }

  test("Drop should return an empty Steam when remove all elements") {
    assert(Stream(1, 2).drop(2).toList == Nil)
  }

  test("Drop should throw an error when n is negative") {
    intercept[IllegalArgumentException] {
      Stream(1, 2).drop(-1)
    }
  }

  test("Drop should return the entire stream when remove no elements") {
    assert(Stream(1, 2).drop(0).toList == Stream(1, 2).toList)
  }

  test("TakeWhile should return all the first element matching a function") {
    assert(Stream(2, 4, 6, 5, 8).takeWhile(_ % 2 == 0).toList == Stream(2, 4, 6).toList)
  }

  test("TakeWhile should return an empty list if the very first element does not match the function") {
    assert(Stream(1, 4).takeWhile(_ % 2 == 0).toList == Stream.empty.toList)
  }

  test("TakeWhile should return the entire list if all elements matches the function") {
    assert(Stream(2, 4).takeWhile(_ % 2 == 0).toList == Stream(2, 4).toList)
  }
}
