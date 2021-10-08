package monad

import cats._
import cats.implicits._
import scala.util.{ Failure, Success, Try }

sealed trait MOption[+A]
object MOption {
  case class MSome[+A](a: A) extends MOption[A]
  case object MNone          extends MOption[Nothing]

  implicit val monadOption = new Monad[MOption] {

    override def flatMap[A, B](fa: MOption[A])(f: A => MOption[B]): MOption[B] = fa match {
      case MSome(a) => f(a)
      case MNone    => MNone
    }

    override def tailRecM[A, B](a: A)(f: A => MOption[Either[A, B]]): MOption[B] =
      pure(a) match {
        case MSome(a) =>
          f(a) match {
            case MSome(a) => a.fold(_ => MNone, right => MSome(right))
            case MNone    => MNone
          }
        case MNone => MNone
      }

    override def pure[A](x: A): MOption[A] =
      MSome(x)

    override def map[A, B](fa: MOption[A])(f: A => B): MOption[B] =
      flatMap(fa)((f andThen pure))
    // flatMap(fa)(a => pure(f(a)))
    // flatMap(fa)(_ match {
    //   case MSome(a: A) => MSome(f(a))
    //   case MNone       => MNone
    // })
    // fa match {
    //   case MSome(a) => MSome(f(a))
    //   case MNone    => MNone
    // }

    override def flatten[A](ffa: MOption[MOption[A]]): MOption[A] =
      flatMap(ffa)(identity)
  }

  implicit val monadList = new Monad[List] {

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa match {
        case h :: t => f(h) ++ flatMap(t)(f)
        case Nil    => Nil
      }

    override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = ???

    override def pure[A](x: A): List[A] =
      List(x)

  }

  implicit def monadEither[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      fa match {
        case Left(value)  => Left(value)
        case Right(value) => f(value)
      }

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???

    override def pure[A](x: A): Either[E, A] = Right(x)

  }

  implicit val monadTry: Monad[Try] = new Monad[Try] {

    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] =
      fa match {
        case Failure(exception) => Failure(exception)
        case Success(value)     => f(value)
      }

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???

    override def pure[A](x: A): Try[A] = Success(x)

  }

}

case class Person(name: String)
case class Account(balance: Double, owner: Person)
case class Transfer(
    source: Account,
    dest: Account,
    amount: Double
  )

object Main extends App {

  def findPersonByName(name: String): MOption[Person]                    = ???
  def findAccountByPerson(person: Person): MOption[Account]              = ???
  def findLastTranserBySourceAccount(accout: Account): MOption[Transfer] = ???
  def findLastTranserByPersonName(name: String): MOption[Transfer] =
    findPersonByName(name)
      .flatMap((person: Person) =>
        findAccountByPerson(person)
          .flatMap(acc => findLastTranserBySourceAccount(acc))
      )
  // findPersonByName(name) match {
  //   case MSome(person) =>
  //     findAccountByPerson(person) match {
  //       case MSome(acc) => findLastTranserBySourceAccount(acc)
  //       case _          => MNone
  //     }
  //   case _ => MNone
  // }

  import MOption._

  val x = Monad[MOption].pure(3)
  val y = x.flatMap(_ => MNone)

  val z = for {
    a <- Monad[MOption].pure(3)
    b <- (MNone: MOption[Int])
  } yield a + b

  val res = for {
    a <- List(1, 2, 3)
    b <- List(4, 5, 6)
  } yield a + b

  monadList.flatMap(List(1, 2, 3))(a => List(a + 1, a + 2))

  val right = 5.asRight.flatMap(x => (x + 1).asRight[String])

  val trySuccess = monadTry.pure(5).flatMap(i => monadTry.pure(i + 1))
  val tryFail    = monadTry.pure(5).flatMap(_ => Failure(new Exception("boom")))
  // fail fast
  val tryFail2 = monadTry
    .pure(5)
    .flatMap(_ =>
      Failure(new Exception("boom"))
        .flatMap(_ => Failure(new Exception("boom 2")))
    )

  // Scala's [[Try]]

  val fail = Success(5).flatMap(_ => throw new Exception("boom"))
  // pure(x).flatMap(f) === f(x)
  // but if the function throws exception
  // and it's called as f(x), then the exception will be thrown
  // but through flatMap, it would wrap error message in [[Failure]] instead
  // breaking the monad law
}
