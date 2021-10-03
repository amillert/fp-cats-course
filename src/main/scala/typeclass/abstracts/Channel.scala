package typeclass
package abstracts

private[typeclass] trait Channel {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
}
