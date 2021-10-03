package typeclass
package instances

import scala.util.Try

private[typeclass] object ByteDecoder {
  import abstracts.ByteDecoder

  def apply[A](implicit ev: ByteDecoder[A]): ByteDecoder[A]    = ev
  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = f(_)

  implicit val stringByteDecoder: ByteDecoder[String] =
    (bytes: Array[Byte]) => Try(new String(bytes)).toOption
}
