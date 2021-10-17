package trampoline

import scala.annotation.tailrec

trait Trampoline[+A]
object Trampoline {

  case class Done[A](a: A) extends Trampoline[A]

  case class More[A](f: () => Trampoline[A]) extends Trampoline[A]

  // reification
  case class FlatMap[A, B](ta: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

  def resume[A](ta: Trampoline[A]): Either[() => Trampoline[A], A] = ta match {
    case Done(a)     => Right(a)
    case More(thunk) => resume(thunk())
    case FlatMap(t, f) =>
      t match {
        case Done(a2) => resume(f(a2))
        case More(t2) => Left(() => FlatMap(t2(), f))
        // thanks to Monad's associativity
        // FlatMap(FlatMap(t2, f2), f) ~~> FlatMap(t2, x => FlatMap(f2(x), f))
        case FlatMap(t2, f2) => resume(FlatMap(t2, (x: Any) => FlatMap(f2(x), f)))
      }
  }

  @tailrec
  def run[A](ta: Trampoline[A]): A =
    resume(ta) match {
      case Right(a)    => a
      case Left(thunk) => run(thunk())
    }
  // ta match {
  //   case Done(a)     => a
  //   case More(thunk) => run(thunk())
  // }
}

object X {
  import Trampoline._

  // Blow up the stack:
  // def isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1)
  // def isOdd(n: Int): Boolean  = if (n == 0) false else isEven(n - 1)

  // trampolined:
  def isEven(n: Int): Trampoline[Boolean] = if (n == 0) Done(true) else More(() => isOdd(n - 1))
  def isOdd(n: Int): Trampoline[Boolean]  = if (n == 0) Done(false) else More(() => isEven(n - 1))

  // non-tailrec:
  // def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
  //   case Nil    => Nil
  //   case h :: t => f(h) ++ flatMap(t)(f)
  // }

  // trampolined:
  def flatMap[A, B](tas: List[A])(f: A => List[B]): Trampoline[List[B]] = tas match {
    case Nil => Done(Nil)
    case h :: t =>
      More { () =>
        FlatMap(flatMap(t)(f), (lb: List[B]) => Done(f(h) ++ lb))
      }
  }
}

object Main extends App {
  import X._

  println(Trampoline.run(isEven(120000)))
  println(Trampoline.run(flatMap((1 to 10000).toList)(i => List(i, i + 1))))
}
