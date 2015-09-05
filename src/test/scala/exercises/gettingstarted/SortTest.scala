package exercises.gettingstarted

import org.scalatest.FunSuite

class SortTest extends FunSuite {

  test("isSorted should return true for a sorted int array") {
    assert(Sort.isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b))
  }

  test("isSorted should return false for a unsorted int array") {
    assert(Sort.isSorted(Array(1, 3, 2), (a: Int, b: Int) => a < b) == false)
  }

  test("isSorted should return true for an empty array") {
    assert(Sort.isSorted(Array(), (a: Int, b: Int) => a < b))
  }
}
