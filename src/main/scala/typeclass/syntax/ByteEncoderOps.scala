package typeclass
package syntax

private[typeclass] object ByteEncoderOps {
  import abstracts.ByteEncoder

  /** Value class improves performance; no new allocations with each implicit */
  implicit class ByteEncoderSyntax[A](val a: A) extends AnyVal {
    def encode(implicit enc: ByteEncoder[A]): Array[Byte] =
      enc.encode(a)
  }
}
