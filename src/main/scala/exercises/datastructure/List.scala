package exercises.datastructure

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def valuePatternMatching =
    List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + List.sum(t)
      case _                                     => 101
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /**
   * Exercise 3.2
   */
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil           => Nil
      case Cons(_, tail) => tail
    }

  /**
   * Exercise 3.3
   */
  def setHead[A](l: List[A], el: A): List[A] =
    l match {
      case Nil           => throw new IllegalArgumentException
      case Cons(_, tail) => Cons(el, tail)
    }

  /**
   * Exercise 3.4
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def loop(l: List[A], n: Int): List[A] =
      if (n == 0) l
      else l match {
        case Nil           => throw new IllegalArgumentException
        case Cons(_, tail) => loop(tail, n - 1)
      }

    loop(l, n)
  }

  /**
   * Exercise 3.5
   */
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(el, tail) =>
        if (f(el)) dropWhile(tail, f)
        else l
    }
  }

  /**
   * Exercise 3.6
   */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil            => throw new IllegalArgumentException
      case Cons(el, Nil)  => Nil
      case Cons(el, tail) => Cons(el, init(tail))
    }

  /**
   * Exercise 3.9
   */
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x, y) => 1 + y)

  /**
   * Exercise 3.10
   */
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  /**
   * Exercise 3.11
   */
  def sumLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def productLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((a, b) => a + 1)

  /**
   * Exercise 3.12
   */
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))
}
