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
}
