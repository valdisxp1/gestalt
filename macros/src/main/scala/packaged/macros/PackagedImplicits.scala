package packaged.macros

import scala.gestalt.defs._

object ImplicitsForNumbers {

  implicit class PlusFor(val a: Int) {
    inline def plus(b: Int): Any = meta {
      q"$this.a + $b"
    }
  }

}