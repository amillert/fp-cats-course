package testing

import cats._
import cats.implicits._

import scala.annotation.tailrec

case class Box[A](value: A)
object Box {
  implicit def eqBoxA[A]: Eq[Box[A]] = new Eq[Box[A]] {
    override def eqv(x: Box[A], y: Box[A]): Boolean = x.value == y.value
  }

  implicit def monadBox[A]: Monad[Box] = new Monad[Box] {

    override def flatMap[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = f(fa.value)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Box[Either[A, B]]): Box[B] =
      f(a).value match {
        case Left(value)  => tailRecM(value)(f)
        case Right(value) => Box(value)
      }
    // f(a).value.fold(tailRecM(_)(f), pure) // not stack-safe

    override def pure[A](x: A): Box[A] = Box(x)

  }
}
