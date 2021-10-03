package typeclasses

object TypeClassesMain extends App {

  /** If this one is found in the implicit scope, it will be used instead */
  implicit object Rot3StringByteEncoder extends ByteEncoder[String] {
    override def encode(s: String): Array[Byte] =
      s.getBytes.map(x => (x + 3).toByte)
  }

  FileChannel.write("hello")
  FileChannel.write(Switch(!false))

  val xd1 = ByteEncoder[String].encode("hello")
  val xd2 = implicitly[ByteEncoder[String]].encode("hello")

  val bytes: Array[Byte] = Array(98, 105, 101, 110, 32, 58, 41)
  val decoded            = ByteDecoder[String].decode(bytes)

  println(decoded)
}
