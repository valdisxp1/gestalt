import scala.collection.immutable.Seq

import scala.gestalt._

object plusObject {
  inline def apply(a: Any, b: Any): Any = meta {
    q"$a + $b"
  }
}


class plus {
  inline def apply(a: Any, b: Any): Any = meta {
    q"$a + $b"
  }
}

class plus2(val a: Int) {
  inline def apply(b: Int): Any = meta {
    q"$this.a + $b"
  }
}

object ImplicitsForNumbers {
  implicit class PlusFor(val a: Int) {
    inline def plus(b: Int): Any = meta {
      q"$this.a + $b"
    }
  }
}

object scope {
  inline def is[T](a: Any): Boolean = meta {
    q"$a.isInstanceOf[$T]"
  }

  inline def both[S, T](a: Any): Boolean = meta {
    q"$a.isInstanceOf[$S] && $a.isInstanceOf[$T]"
  }
}

object trees {
 /* inline def some3: Option[Int] = meta {
    q"Some(3)"
  }
*/
  inline def five(aTree: Any): Any = meta {
    val five = q"5" //XXX five is an actual number 5, NOT the corresponding tree
    println(s">>> FIVE $five ${five + 3}")
    // just something so the macro will compile
    q"$aTree"
  }
  /*inline def constant: Double = meta{
    q"Math.Pi"
  }*/
  inline def ident(a: Any): Any = meta{
    q"$a"
  }
}