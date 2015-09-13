package exercises.handlingerrors

trait Option[+A] {

  /**
   * Exercise 4.1
   */
  def map[B](f: A => B): Option[B] =
    this match {
      case None    => None
      case Some(v) => Some(f(v))
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None    => default
      case Some(v) => v
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    map(f).flatMap(if (_) this else None)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  /**
   * Exercise 4.3
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.flatMap(y => Some(f(x, y))))

  /**
   * Exercise 4.4
   */
  def sequence[A](l: List[Option[A]]): Option[List[A]] = {
    l.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a, b)(_ :: _))
  }
}
