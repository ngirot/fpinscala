package exercises.strictnesslaziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("ToList should convert a Stream into a List") {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
  }

  test("ToList of an empty Stream is an empty List") {
    assert(Empty.toList == Nil)
  }
}
