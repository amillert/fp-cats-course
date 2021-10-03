package typeclass
package instances

import java.nio.ByteBuffer

private[typeclass] object ByteCodec {
  import abstracts.ByteCodec

  implicit val intByteCodec: ByteCodec[Int] = new ByteCodec[Int] {
    override def decode(bytes: Array[Byte]): Option[Int] =
      if (bytes.length != 4) None
      else {
        val bb = ByteBuffer.allocate(4)
        bb.put(bytes) // write mode
        bb.flip()     // flip to read mode
        Some(bb.getInt)
      }

    override def encode(a: Int): Array[Byte] = {
      val bb = ByteBuffer.allocate(4)
      bb.putInt(a)
      bb.array()
    }
  }
}
