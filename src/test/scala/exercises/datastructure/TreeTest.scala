package exercises.datastructure

import org.scalatest.FunSuite

class TreeTest extends FunSuite {

  test("Size should return 1 for a tree with just one leaf") {
    assert(Tree.size(Leaf(1)) == 1)
  }

  test("Size should return 5 for a tree with just tree branches and two leafs") {
    assert(Tree.size(Branch(Branch(Leaf(1), Leaf(1)), Leaf(2))) == 5)
  }

  test("Maximum sould return the biggest integer of a tree") {
    assert(Tree.maximum(Branch(Branch(Leaf(-10), Leaf(10)), Leaf(8))) == 10)
  }

  test("Depth of a leaf should be one") {
    assert(Tree.depth(Leaf(5)) == 1)
  }

  test("Depth of a tree with branch should the longest path to a leaf") {
    assert(Tree.depth(Branch(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1)), Leaf(1))) == 5)
  }

  test("Map should create a Tree with transformed values from a function") {
    assert(Tree.map(Branch(Branch(Leaf(1), Leaf(1)), Leaf(2)))(_ + 1) == Branch(Branch(Leaf(2), Leaf(2)), Leaf(3)))
  }

  test("Fold should transform leafs and branches with functions") {
    assert(Tree.fold(Branch(Branch(Leaf(1), Leaf(1)), Leaf(2)))(_ + 1)(_ + _) == 7)
  }

  test("SizeUsingFold should return 1 for a tree with just one leaf") {
    assert(Tree.sizeUsingFold(Leaf(1)) == 1)
  }

  test("SizeUsingFold should return 5 for a tree with just tree branches and two leafs") {
    assert(Tree.sizeUsingFold(Branch(Branch(Leaf(1), Leaf(1)), Leaf(2))) == 5)
  }

  test("MaximumUsingFold sould return the biggest integer of a tree") {
    assert(Tree.maximumUsingFold(Branch(Branch(Leaf(-10), Leaf(10)), Leaf(8))) == 10)
  }

  test("DepthUsingFold of a leaf should be one") {
    assert(Tree.depthUsingFold(Leaf(5)) == 1)
  }

  test("DepthUsingFold of a tree with branch should the longest path to a leaf") {
    assert(Tree.depthUsingFold(Branch(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1)), Leaf(1))) == 5)
  }

  test("MapUsingFold should create a Tree with transformed values from a function") {
    assert(Tree.mapUsingFold(Branch(Branch(Leaf(1), Leaf(1)), Leaf(2)))(_ + 1) == Branch(Branch(Leaf(2), Leaf(2)), Leaf(3)))
  }

}
