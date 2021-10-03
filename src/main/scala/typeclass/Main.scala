package typeclass

import typeclass.abstracts._
import typeclass.instances._

object Main extends App {

  /** If this one is found in the implicit scope, it will be used instead */
  implicit object Rot3StringByteEncoder extends ByteEncoder[String] {
    override def encode(s: String): Array[Byte] =
      s.getBytes.map(x => (x + 3).toByte)
  }

  FileChannel.write("hello")

  import typeclass.instances.ByteEncoder._
  FileChannel.write(Switch(!false))

  val xd1 = ByteEncoder[String].encode("hello")
  val xd2 = implicitly[ByteEncoder[String]].encode("hello")

  val bytes: Array[Byte] = Array(98, 105, 101, 110, 32, 58, 41)

  import typeclass.instances.ByteDecoder._
  val decoded = ByteDecoder[String].decode(bytes)

  println(decoded)

  ByteEncoder[Option[String]].encode(Some("world"))
  ByteEncoder[Option[String]].encode(None)

  import syntax.ByteEncoderOps._

  "string".encode

  import syntax.ByteDecoderOps._

  val someOpt = Array[Byte](0, 0, 0, 5).decode
  val noneOpt = Array[Byte](0, 0, 0, 0, 5).decode
}
