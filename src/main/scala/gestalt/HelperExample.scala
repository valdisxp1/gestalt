package gestalt

import scala.gestalt.{Toolbox, TypeToolbox}

class HelperExample[T<:Toolbox](val toolbox: T) {
  def intArrayLit(array: Seq[Int]): toolbox.Tree = toolbox.Apply(toolbox.Ident("Array"),array.toList.map(toolbox.Lit))
}

class HelperExample2[T <: TypeToolbox](val toolbox: T) {
  def intArrayLit(array: Seq[Int]): toolbox.Tree = toolbox.Apply(toolbox.Ident("Array"), array.toList.map(toolbox.Lit))
  object Varargs{
    def unapply(tree: toolbox.Tree): Option[Either[toolbox.Tree,Seq[toolbox.Tree]]] = tree match {
      case toolbox.SeqLiteral(seq) => Some(Right(seq))
      case toolbox.Ascribe(seq,_) => Some(Left(seq))
      case _ => None
    }
  }
}

