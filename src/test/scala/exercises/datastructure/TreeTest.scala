package exercises.datastructure

import org.scalatest.FunSuite

class TreeTest extends FunSuite {

  test("Size should return 1 for a tree with just one leaf") {
    assert(Tree.size(Leaf(1)) == 1)
  }

  test("Size should return 5 for a tree with just tree branches and two leafs") {
    assert(Tree.size(Branch(Branch(Leaf(1), Leaf(1)), Leaf(2))) == 5)
  }

}
