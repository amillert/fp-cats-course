package typeclasses

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
  def instance[A](f: A => Array[Byte]): ByteEncoder[A]      = f(_)

  implicit val stringByteEncoder: ByteEncoder[String] = instance(_.getBytes)

  implicit val stringRot3ByteEncoder: ByteEncoder[String] =
    instance(_.getBytes.map(x => (x + 3).toByte))
}
