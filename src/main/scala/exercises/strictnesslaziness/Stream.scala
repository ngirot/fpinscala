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

  /**
   * Exercise 5.13
   */

  def mapUsingUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this)(x => x match {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    })

  def takeUsingUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n))(x => x match {
      case (_, n) if n < 0           => throw new IllegalArgumentException
      case (Cons(h, t), n) if n == 1 => Some((h()), (Stream.empty, 0))
      case (Cons(h, t), n) if n > 1  => Some((h()), (t(), n - 1))
      case _                         => None
    })

  def takeWhileUsingUnfold(f: A => Boolean): Stream[A] =
    Stream.unfold(this)(x => x match {
      case Cons(h, t) if (f(h())) => Some((h(), t()))
      case _                      => None
    })

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s))(x => x match {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2))(x => x match {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h, t), Empty)          => Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(h, t))          => Some((None, Some(h())), (Empty, t()))
      case _                            => None
    })
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

  /**
   * Exercise 5.12
   */

  def onesUsingUnfold: Stream[Int] =
    unfold(1)(x => Some(1, 1))

  def constantUsingUnfold[A](a: A): Stream[A] =
    unfold(a)(x => Some(a, a))

  def fromUsingUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x + 1))

  def fibsUsingUnfold: Stream[Int] =
    unfold((0, 1))(x => Some(x._1, (x._2, x._1 + x._2)))

}
