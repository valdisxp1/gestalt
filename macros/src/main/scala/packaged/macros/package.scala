package packaged

import scala.gestalt.defs._

package object macros {
  inline def plus(a: Int, b: Int): Int = meta {
    q"$a + $b"
  }

  object PlusObj {
    inline def plus(a: Int, b: Int): Int = meta {
      q"$a + $b"
    }
  }

  implicit class PlusFor(val a: Int) {
    inline def plus(b: Int): Int = meta {
      q"$this.a + $b"
    }
  }
}
