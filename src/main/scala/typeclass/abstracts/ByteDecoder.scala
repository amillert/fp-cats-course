package typeclass
package abstracts

private[typeclass] trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}
