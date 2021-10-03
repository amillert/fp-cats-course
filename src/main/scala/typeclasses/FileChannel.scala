package typeclasses

import java.io.FileOutputStream
import scala.util.Using

object FileChannel extends Channel {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit =
    Using(new FileOutputStream("/home/devmood/private/fp-course/tmp")) { os =>
      os.write(enc.encode(obj))
      os.flush()
    }
}
