package monad

import cats._
import cats.implicits._

sealed trait MOption[+A] {

  // def flatMap[A, B](f: A => MOption[B]): MOption[B] = {
  //   import MOption._

  //   this match {

  //     case MSome(a: A) => f(a)
  //     case MNone       => MNone
  //   }
  // }
}
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
}
