package exercises.datastructure;

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
   * Exercise 3.25
   */
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_)      => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }

  /**
   * Exercise 3.26
   */
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(value)  => value
      case Branch(l, r) => maximum(l) max maximum(r)
    }
}
