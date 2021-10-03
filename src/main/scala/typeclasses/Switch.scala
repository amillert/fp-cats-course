package typeclasses

case class Switch(isOn: Boolean)

object Switch {
  implicit object SwitchByteEncoder extends ByteEncoder[Switch] {
    override def encode(s: Switch): Array[Byte] =
      Array(if (s.isOn) '1'.toByte else '0'.toByte)
  }
}
