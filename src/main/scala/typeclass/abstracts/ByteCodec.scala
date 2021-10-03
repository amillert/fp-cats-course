package typeclass
package abstracts

/** Encoding and decoding should return the original value */
private[typeclass] trait ByteCodec[A] extends ByteEncoder[A] with ByteDecoder[A]
