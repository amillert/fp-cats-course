package typeclasses

trait Channel {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
}
