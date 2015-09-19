package exercises.strictnesslaziness

import scala.annotation.tailrec

sealed trait Stream[+A] {

  /**
   * Exercise 5.1
   */
  def toList: List[A] =
    this match {
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList
    }

  /**
   * Exercise 5.2
   */
  def take(n: Int): Stream[A] =
    this match {
      case _ if n < 0           => throw new IllegalArgumentException
      case Cons(h, _) if n == 1 => Cons(h, () => Empty: Stream[A])
      case Cons(h, t) if n > 1  => Cons(h, () => t().take(n - 1))
      case _                    => Empty
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
