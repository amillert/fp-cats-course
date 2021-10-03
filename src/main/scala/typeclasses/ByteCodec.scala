package typeclasses

/** Encoding and decoding should return the original value */
trait ByteCodec[A] extends ByteEncoder[A] with ByteDecoder[A]
