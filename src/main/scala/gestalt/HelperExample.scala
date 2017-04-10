package example.helpers

import scala.gestalt.{Toolbox, TypeToolbox}

class HelperExample[T<:Toolbox](val tb: T) {
  def intArrayLit(array: Int*): tb.Tree = tb.Apply(tb.Ident("Array"),array.toList.map(tb.Lit))
}

class HelperExample2[T <: TypeToolbox](val tb: T) {
  def intArrayLit(array: Int*): tb.Tree = tb.Apply(tb.Ident("Array"), array.toList.map(tb.Lit))
  object Varargs{
    def unapply(tree: tb.Tree): Option[Either[tb.Tree,Seq[tb.Tree]]] = tree match {
      case tb.SeqLiteral(seq) => Some(Right(seq))
      case tb.Ascribe(seq,_) => Some(Left(seq))
      case _ => None
    }
  }
}

