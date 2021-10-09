package traverse

import cats._
import cats.implicits._
import cats.instances.option

trait MList[+A]
object MList {
  def traverse[F[_]: Applicative, A, B](as: MList[A])(f: A => F[B]): F[MList[B]] =
    as match {
      case MNil        => Applicative[F].pure(MNil)
      case MCons(h, t) => (f(h), traverse(t)(f)).mapN(MCons.apply)
    }
}
case class MCons[+A](h: A, t: MList[A]) extends MList[A]
case object MNil                        extends MList[Nothing]

object Main extends App {
  val sequenced: Option[List[Int]] = Traverse[List].sequence(List(Option(1), Option(2)))
  println(sequenced) // Some(List(1, 2))
  val sequencedWithNone: Option[List[Int]] = Traverse[List].sequence(List(Option(1), None))
  println(sequencedWithNone) // None

  val traversed = Traverse[List].traverse(List(1, 2, 3))(i => Option(i + 1))
  println(traversed) // Some(List(2, 3, 4))

  val optionTraverse: Traverse[Option] = new Traverse[Option] {

    override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
      case Some(value) => f(b, value)
      case None        => b
    }

    override def foldRight[A, B](fa: Option[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match {
        case Some(value) => f(value, lb)
        case None        => lb
      }

    override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
      fa match {
        case Some(value) => f(value).map(Some.apply)
        case None        => Applicative[G].pure(None: Option[B])
      }
    // foldLeft(fa, Applicative.pure[Option])((acc, a) => f(a))

  }

  println(optionTraverse.traverse(Some(5))(x => List(x + 1, x * 10))) // List(Some(6), Some(50))
  println(optionTraverse.traverse(Some(5))(x => List()))              // List()
  println(optionTraverse.traverse(None)(_ => List()))                 // List(None)
  // println(optionTraverse.traverse(None)(x => List(x + 1)))            // x is of type Nothing
  println(optionTraverse.traverse[List, Int, Int](None)(x => List(x + 1))) // List(None)
}
