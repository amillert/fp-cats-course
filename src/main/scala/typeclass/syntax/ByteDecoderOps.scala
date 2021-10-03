package typeclass
package syntax

private[typeclass] object ByteDecoderOps {
  import abstracts.ByteDecoder

	/** Value class improves performance; no new allocations with each implicit */
  implicit class ByteDecoderSyntax[A](val bytes: Array[Byte]) extends AnyVal {
    def decode(implicit dec: ByteDecoder[A]) = dec.decode(bytes)
  }
}
