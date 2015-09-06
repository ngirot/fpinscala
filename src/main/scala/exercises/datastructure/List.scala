package exercises.datastructure

import scala.annotation.tailrec
import scala.util.control.TailCalls.TailRec

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

  /**
   * Exercise 3.13
   */

  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a, b) => f(b, a))

  def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  /**
   * Exercise 3.14
   */
  def append[A](a: List[A], b: List[A]): List[A] =
    foldRight(a, b)((a, b) => Cons(a, b))

  /**
   * Exercise 3.15
   */
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  /**
   * Exercise 3.16
   */
  def addOne[A](l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  /**
   * Exercise 3.17
   */
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  /**
   * Exercise 3.18
   */
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  /**
   * Exercise 3.19
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  /**
   * Exercise 3.20
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  /**
   * Exercise 3.21
   */
  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)((a) => if (f(a)) List(a) else Nil)

  /**
   * Exercise 3.22
   */
  def addTwoLists(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (_, Nil)                             => Nil
      case (Nil, _)                             => Nil
      case (Cons(el1, tail1), Cons(el2, tail2)) => Cons(el1 + el2, addTwoLists(tail1, tail2))
    }

  /**
   * Exercise 3.23
   */
  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] =
    (l1, l2) match {
      case (_, Nil)                             => Nil
      case (Nil, _)                             => Nil
      case (Cons(el1, tail1), Cons(el2, tail2)) => Cons(f(el1, el2), zipWith(tail1, tail2)(f))
    }

  /**
   * Exercise 3.24
   */
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =
    l match {
      case Nil => false
      case Cons(el, tail) if length(filter(zipWith(l, sub)(_ == _))(_ == false)) == 0 => true
      case Cons(el, tail) => hasSubsequence(tail, sub)
    }
}
