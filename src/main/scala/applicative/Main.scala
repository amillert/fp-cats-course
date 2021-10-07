package applicative

import cats._
import cats.implicits._

sealed trait Validated[+A]
object Validated {
  case class Valid[+A](value: A)           extends Validated[A]
  case class Invalid(errors: List[String]) extends Validated[Nothing]

  implicit val applicative: Applicative[Validated] = new Applicative[Validated] {
    override def ap[A, B](ff: Validated[A => B])(fa: Validated[A]): Validated[B] = (ff, fa) match {
      case (Valid(f), Valid(a))     => Valid(f(a))
      case (Valid(f), Invalid(e))   => Invalid(e)
      case (Invalid(e), Valid(a))   => Invalid(e)
      case (Invalid(f), Invalid(a)) => Invalid(f ++ a)
    }

    // def appInTermsOfAp2[A, B](ff: Validated[A => B])(fa: Validated[A]): Validated[B] = ???

    override def pure[A](x: A): Validated[A] = Valid(x)

    override def ap2[A, B, Z](
        ff: Validated[(A, B) => Z]
      )(
        fa: Validated[A],
        fb: Validated[B]
      ): Validated[Z] =
      (fa, fb) match {
        case (Valid(a), Valid(b)) =>
          val Valid(f) = ff
          Valid(f(a, b))
        case (Valid(a), Invalid(eb))    => Invalid(eb)
        case (Invalid(ea), Valid(b))    => Invalid(ea)
        case (Invalid(ea), Invalid(eb)) => Invalid(ea ++ eb)

      }
  }
}

object Main extends App {}
