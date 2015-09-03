package exercices.datastructure

object FoldRight {

  /**
   * Exercice 3.8
   */
  def main(args: Array[String]) = {
    // Should rebuild the initial list
    val result = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    print(result)
  }
}
