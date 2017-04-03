package packaged.macros
import scala.gestalt.defs._

object InnerObjectMacro {
  class Inner {
    inline def plus(a: Int, b: Int) = meta {
      q"$a + $b"
    }
  }

  object InnerObject {
    inline def plus(a: Int, b: Int) = meta {
      q"$a + $b"
    }
  }

}
