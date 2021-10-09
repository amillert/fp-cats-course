package foldable

import cats._
import cats.implicits._
import scala.annotation.tailrec

trait MList[+A]
object MList {

  implicit val foldableList: Foldable[MList] = new Foldable[MList] {

    override def foldLeft[A, B](fa: MList[A], b: B)(f: (B, A) => B): B = fa match {
      case MNil        => b
      case MCons(h, t) => foldLeft(t, f(b, h))(f)
    }

    override def foldRight[A, B](fa: MList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def go(xs: MList[A]): Eval[B] = xs match {
        case MCons(h, t) => f(h, Eval.defer(go(t)))
        case MNil        => lb
      }

      Eval.defer(go(fa))
    }

  }

  def foldRight[A, B](xs: MList[A])(z: B)(f: (A, B) => B): B =
    xs match {
      case MNil => z
      // associates to right
      // must be deferred to be stack-safety but preserves the order
      // pretty much representing the recursive data structure itself
      case MCons(h, t) => f(h, foldRight(t)(z)(f))
      // case MCons(h, t) => foldRight(t)(f(h, z))(f)
    }

  @tailrec
  def foldLeft[A, B](xs: MList[A])(z: B)(f: (B, A) => B): B =
    xs match {
      case MNil => z
      // associates to left - natural but reverses the order
      // stack-safe since tail-recursive
      case MCons(h, t) => foldLeft(t)(f(z, h))(f)
    }

}
case class MCons[+A](h: A, t: MList[A]) extends MList[A]
case object MNil                        extends MList[Nothing]

object Main extends App {
  def sum(xs: MList[Int]): Int = xs match {
    case MNil        => 0
    case MCons(h, t) => h + sum(t)
  }

  def sumFold(xs: MList[Int]): Int =
    MList.foldRight(xs)(0)(_ + _)

  def length(xs: MList[Int]): Int = xs match {
    case MNil        => 0
    case MCons(_, t) => 1 + length(t)
  }

  def lengthFold(xs: MList[Int]): Int =
    MList.foldRight(xs)(0)((_, acc) => acc + 1)

  def filterPositive(xs: MList[Int]): MList[Int] = xs match {
    case MNil                 => MNil
    case MCons(h, t) if h > 0 => MCons(h, filterPositive(t))
    case MCons(_, t)          => filterPositive(t)
  }

  def filterPositiveFold(xs: MList[Int]): MList[Int] =
    MList.foldRight(xs)(MNil: MList[Int])((x, acc) => if (x > 0) MCons(x, acc) else acc)

  val cons = MCons(1, MCons(2, MCons(3, MNil)))
  println(sum(cons))
  println(length(cons))
  println(filterPositive(MCons(-2, cons)))

  println(MList.foldRight(cons)(0)(_ + _))
  println(MList.foldLeft(cons)(0)((acc, _) => 1 + acc))
  // reversed order
  println(MList.foldLeft(cons)(MNil: MList[Int])((acc, x) => if (x > 0) MCons(x, acc) else acc))

  import MList.foldableList

  println(foldableList.fold(cons))
  println(foldableList.fold(MCons("hello", MCons(" world!", MNil))))
  println(foldableList.foldMap(MCons("hello", MCons(" world!", MNil)))(_.show))
  println(foldableList.foldMap(cons)(_.show))
  println(foldableList.foldMap(cons)(_ + 2)) // add 2 to each and combine

  // it's not find-first operation
  def find[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Option[A] =
    fa.foldLeft(None: Option[A])((acc, x) => if (p(x)) Some(x) else acc)

  println(find[MList, Int](cons)(_ > 1))

  def exists[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean =
    // find(fa)(p).nonEmpty
    fa.foldLeft(false)((acc, x) => acc || p(x))

  println(exists[MList, Int](cons)(_ > 2))
  println(exists[MList, Int](cons)(_ < 1))

  def toListLeft[F[_]: Foldable, A](fa: F[A]): List[A] =
    // if `:+` operator wasn't used, it'd be reversed
    fa.foldLeft(List.empty[A])((acc, x) => acc :+ x)

  def toListRight[F[_]: Foldable, A](fa: F[A]): List[A] =
    fa.foldRight(Eval.now(List.empty[A]))((x, acc) => Eval.now(x :: acc.value)).value

  println(toListLeft[MList, Int](cons))
  println(toListRight[MList, Int](cons))

  def forAll[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean =
    fa.foldLeft(true)((acc, x) => acc && p(x))

  println(forAll[MList, Int](cons)(_ > 0))
}
