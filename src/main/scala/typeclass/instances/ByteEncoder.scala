package typeclass
package instances

private[typeclass] object ByteEncoder {
  import abstracts.ByteEncoder

  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
  def instance[A](f: A => Array[Byte]): ByteEncoder[A]      = f(_)

  implicit val stringByteEncoder: ByteEncoder[String] = instance(_.getBytes)

  implicit val stringRot3ByteEncoder: ByteEncoder[String] =
    instance(_.getBytes.map(x => (x + 3).toByte))

  implicit val switchByteEncoder: ByteEncoder[Switch] =
    (s: Switch) => Array(if (s.isOn) '1'.toByte else '0'.toByte)

  implicit def optionEncoder[A](implicit enc: ByteEncoder[A]): ByteEncoder[Option[A]] = {
    case Some(x) => enc.encode(x)
    case None    => Array.empty[Byte]
  }

  // implicit val optionStringByteEncoder: ByteEncoder[Option[String]] = {
  //   case Some(s) => stringByteEncoder.encode(s)
  //   case None    => Array.empty[Byte]
  // }
  // (a: Option[String]) => a.getOrElse("none").getBytes
}
