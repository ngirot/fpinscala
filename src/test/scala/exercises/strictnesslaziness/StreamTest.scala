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
    assert(Stream(1, 2, 3, 4, 5).take(3).toList == List(1, 2, 3))
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
    assert(Stream(1, 2).take(3).toList == List(1, 2))
  }

  test("Drop should return a Stream without its nth elements") {
    assert(Stream(1, 2, 3, 4, 5).drop(3).toList == List(4, 5))
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
    assert(Stream(1, 2).drop(0).toList == List(1, 2))
  }

  test("TakeWhile should return all the first element matching a function") {
    assert(Stream(2, 4, 6, 5, 8).takeWhile(_ % 2 == 0).toList == List(2, 4, 6))
  }

  test("TakeWhile should return an empty list if the very first element does not match the function") {
    assert(Stream(1, 4).takeWhile(_ % 2 == 0) == Empty)
  }

  test("TakeWhile should return the entire list if all elements matches the function") {
    assert(Stream(2, 4).takeWhile(_ % 2 == 0).toList == List(2, 4))
  }

  test("ForAll should return true when all elements matches") {
    assert(Stream(2, 4).forAll(_ % 2 == 0) == true)
  }

  test("ForAll should return false when one of the elements doesn't matche") {
    assert(Stream(2, 3, 6).forAll(_ % 2 == 0) == false)
  }

  test("ForAll should return true for an empty Stream") {
    assert(Empty.forAll(null) == true)
  }

  test("TakeWhileUsingFoldRight should return all the first element matching a function") {
    assert(Stream(2, 4, 6, 5, 8).takeWhileUsingFoldRight(_ % 2 == 0).toList == List(2, 4, 6))
  }

  test("TakeWhileUsingFoldRight should return an empty list if the very first element does not match the function") {
    assert(Stream(1, 4).takeWhileUsingFoldRight(_ % 2 == 0) == Empty)
  }

  test("TakeWhileUsingFoldRight should return the entire list if all elements matches the function") {
    assert(Stream(2, 4).takeWhileUsingFoldRight(_ % 2 == 0).toList == List(2, 4))
  }

  test("HeadOptionUsingFoldRight should return None when the Stream is Empty") {
    assert(Stream.empty.headOptionUsingFoldRight == None)
  }

  test("HeadOptionUsingFoldRight should return Some  of the first element when the Stream is not Empty") {
    assert(Stream(1, 2).headOptionUsingFoldRight == Some(1))
  }

  test("Map on an empty stream should return an empty stream") {
    assert(Empty.map(null) == Empty)
  }

  test("Map should apply fonction all on element on a Stream") {
    assert(Stream(1, 2, 3).map(_ + 1).toList == List(2, 3, 4))
  }

  test("Filter should create a stream without all element match a function") {
    assert(Stream(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0).toList == List(2, 4, 6))
  }

  test("Append should create a stream with the elements of two others in order") {
    assert(Stream(1, 2).append(Stream(3, 4)).toList == List(1, 2, 3, 4))
  }

  test("Append an empty stream to another stream should return the other stream") {
    assert(Empty.append(Stream(1, 2)).toList == List(1, 2))
  }

  test("Append a list to an empty list should return the initial list") {
    assert(Stream(1, 2).append(Empty).toList == List(1, 2))
  }

  test("Append an empty list to another empty list should be an emtpy list") {
    assert(Empty.append(Empty) == Empty)
  }

  test("FlatMap should retun a stream from all element created from the fonction") {
    assert(Stream(1, 2).flatMap(el => Stream(el, el)).toList == List(1, 1, 2, 2))
  }

}
