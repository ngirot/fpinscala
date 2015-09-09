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
