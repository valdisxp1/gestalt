package packaged.macros

import scala.gestalt.defs._

class InnerClassMacro {
  def createInner = new Inner()
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
