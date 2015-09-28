package exercises.strictnesslaziness

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  def headOption: Option[A] =
    this match {
      case Empty      => None
      case Cons(h, t) => Some(h())
    }

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

  /**
   * Exercise 5.4
   */
  def forAll(p: A => Boolean): Boolean =
    this match {
      case Empty      => true
      case Cons(h, t) => p(h()) && t().forAll(p)
    }

  /**
   * Exercise 5.5
   */
  def takeWhileUsingFoldRight(f: A => Boolean): Stream[A] = {
    def buildTail(a: A, b: Stream[A]): Stream[A] = {
      if (f(a)) Stream.cons(a, b.takeWhileUsingFoldRight(f))
      else Stream.empty
    }
    foldRight(Stream.empty[A])((a, b) => buildTail(a, b))
  }

  /**
   * Exercise 5.6
   */
  def headOptionUsingFoldRight: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  /**
   * Exercise 5.7
   */
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))

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

  /**
   * Exercise 5.8
   */
  def constant[A](a: A): Stream[A] = {
    lazy val stream: Stream[A] = Cons(() => a, () => stream)
    stream
  }

  /**
   * Exercise 5.9
   */
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  /**
   * Exercise 5.10
   */
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a + b))
    go(0, 1)
  }

  /**
   * Exercise 5.11
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    val opt = f(z)
    return opt match {
      case None    => empty
      case Some(x) => cons(x._1, unfold(x._2)(f))
    }
  }
}
