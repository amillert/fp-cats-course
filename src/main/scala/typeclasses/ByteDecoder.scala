package typeclasses

import scala.util.Try

trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}

object ByteDecoder {
  def apply[A](implicit ev: ByteDecoder[A]): ByteDecoder[A]    = ev
  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = f(_)

  implicit val stringByteDecoder: ByteDecoder[String] =
    (bytes: Array[Byte]) => Try(new String(bytes)).toOption
}
