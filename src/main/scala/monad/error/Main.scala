package monad
package error

import java.io.IOException
import scala.util.{ Failure, Success, Try }
import cats.MonadError

trait HttpMethod
object HttpMethod {

  def doRequest(req: HttpRequest): HttpResponse =
    if (math.random() < 0.5) throw new IOException("boom!") else HttpResponse(200)

  def executeRequestOption(req: HttpRequest): Option[HttpResponse] =
    Try(Some(doRequest(req))).fold(_ => None, identity)

  def executeRequestTry(req: HttpRequest): Try[HttpResponse] =
    Try(doRequest(req))

  def executeRequestEither(req: HttpRequest): Either[String, HttpResponse] =
    Try(doRequest(req)).toEither.fold(_ => Left("error"), Right.apply)
}
case object GET extends HttpMethod
case class HttpRequest(method: HttpMethod, url: String)
case class HttpResponse(status: Int)

object MonadErrorOps {
  import HttpMethod._

  implicit val optionME: MonadError[Option, Unit] = new MonadError[Option, Unit] {

    override def pure[A](x: A): Option[A] = Some(x)

    override def raiseError[A](e: Unit): Option[A] = None

    override def handleErrorWith[A](fa: Option[A])(f: Unit => Option[A]): Option[A] =
      fa.orElse(f(()))
    // fa match {
    //   case Some(value) => Some(value)
    //   case None        => f(())
    // }

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???

  }

  implicit def eitherME[E]: MonadError[Either[E, *], E] = new MonadError[Either[E, *], E] {

    override def pure[A](x: A): Either[E, A] = Right(x)

    override def raiseError[A](e: E): Either[E, A] = Left(e)

    override def handleErrorWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] =
      fa.fold(f(_), Right.apply)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???

  }

  implicit def tryME: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {

    override def pure[A](x: A): Try[A] = Success(x)

    override def raiseError[A](e: Throwable): Try[A] = Failure(e)

    override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] =
      fa.fold(f, Success.apply)

    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???

  }

  def executeRequest[F[_]](
      request: HttpRequest
    )(implicit
      ME: MonadError[F, Throwable]
    ): F[HttpResponse] = Try(ME.pure(HttpMethod.doRequest(request))).fold(ME.raiseError, identity)

  def executeRequest2[F[_], E](
      request: HttpRequest
    )(
      f: Throwable => E
    )(implicit
      ME: MonadError[F, E]
    ): F[HttpResponse] =
    Try(ME.pure(HttpMethod.doRequest(request)))
      .fold(e => ME.raiseError(f(e)), identity)

}

object Main extends App {
  import MonadErrorOps._

  type ThrowErrorOr[A] = Either[Throwable, A]
  executeRequest[ThrowErrorOr](HttpRequest(GET, "www.google.com"))

  executeRequest2[Option, Unit](HttpRequest(GET, "www.google.com"))((e: Throwable) => ())

  type StrErrorOr[A] = Either[String, A]
  executeRequest2[StrErrorOr, String](HttpRequest(GET, "www.google.com"))((e: Throwable) =>
    e.getMessage
  )

  val successOpt: Option[Either[Unit, Int]]  = MonadError[Option, Unit].attempt(Some(5))
  val failOpt: Option[Either[Unit, Nothing]] = MonadError[Option, Unit].attempt(None)

  val successTry = MonadError[Try, Throwable].attempt(Success(5))
  val failTry    = MonadError[Try, Throwable].attempt(Failure(new Exception("boom")))

  val none = MonadError[Option, Unit].ensure(Some(3))(())(_ % 2 == 0)
  val some = MonadError[Option, Unit].ensure(Some(4))(())(_ % 2 == 0)

  val left  = MonadError[StrErrorOr, String].ensure(Right(3))("boom")(_ % 2 == 0)
  val right = MonadError[StrErrorOr, String].ensure(Right(4))("boom")(_ % 2 == 0)

}
