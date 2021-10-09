package functor

import cats._
import cats.implicits._

case class Secret[A](value: A) {
  private def hashed: String    = value.hashCode.toString
  override def toString: String = hashed
}

object Secret {
  implicit val secretFunctor = new Functor[Secret] {
    override def map[A, B](fa: Secret[A])(f: A => B): Secret[B] =
      new Secret(f(fa.value))
  }
}

object Main extends App {
  val secret: Secret[String] = new Secret("value")
  println(secret)
  println(secret.value)

  println(Functor[Secret].map(secret)(_.toUpperCase).value)
  // println(map(_.toUpperCase).value)

  val optionFunctor = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(value) => Some(f(value))
        case None        => None
      }
  }
  val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa match {
        case h :: t => f(h) :: map(t)(f)
        case Nil    => Nil
      }
  }
  println(optionFunctor.map(Some(3))(_ + 1))

  println(optionFunctor.as(Some(12), 10))
  println(listFunctor.as(List(12, 13, 14), 10))
}
