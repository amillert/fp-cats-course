package typeclass
package abstracts

private[typeclass] trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}
