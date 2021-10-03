package typeclass
package instances

import java.io.FileOutputStream
import scala.util.Using

private[typeclass] object FileChannel extends abstracts.Channel {
  import abstracts.ByteEncoder

  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit =
    Using(new FileOutputStream("/home/devmood/private/fp-course/tmp")) { os =>
      os.write(enc.encode(obj))
      os.flush()
    }
}
