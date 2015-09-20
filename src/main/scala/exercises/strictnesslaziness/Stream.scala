package exercises.strictnesslaziness

import scala.annotation.tailrec
import scala.util.control.TailCalls.TailRec

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
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case Cons(h, t) if n > 1  => Stream.cons(h(), t().take(n - 1))
      case _                    => Stream.empty
    }

  @tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case _ if n < 0  => throw new IllegalArgumentException
      case Empty       => Empty
      case _ if n == 0 => this
      case Cons(_, t)  => t().drop(n - 1)
    }

  /**
   * Exercise 5.3
   */
  def takeWhile(f: (A) => Boolean): Stream[A] =
    this match {
      case Empty                  => Empty
      case Cons(h, t) if (f(h())) => Stream.cons(h(), t().takeWhile(f))
      case _                      => Stream.empty
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
