package scala

package object gestalt {
  private trait helper {
    /** The dummy Toolbox for type checking
      */
    val toolbox: Toolbox

    /** The marker for meta block to highlight the different semantics
      */
    def meta(body: toolbox.Tree): Nothing = ???

    /** Placeholder of quasiquotes for type checking
      */
    implicit class QuasiquoteHelper(val sc: StringContext) {

      object q {
        def apply(args: Any*): toolbox.Tree = ???

        def unapply(tree: toolbox.Tree): Any = ???
      }

      object t {
        def apply(args: Any*): toolbox.Tree = ???

        def unapply(tree: toolbox.Tree): Any = ???
      }

    }

  }

  object defs extends helper {
    val toolbox: TypeToolbox = null
  }

  object annotations extends helper {
    val toolbox: StructToolbox = null
  }

  /** Quasiquote implementation based on standard constructors and extractors
   *
   *  This method is intended to be reflectively called by the compiler
   */
  def expand(t: Toolbox)(label: String, tree: t.Tree, parts: List[String], unquotes: List[t.Tree], isPattern: Boolean): t.Tree = {
    val quote      = new Quasiquote(t, "toolbox")
    val argsTyped  = unquotes.asInstanceOf[List[quote.t.Tree]]
    quote.expand(label, tree.asInstanceOf[quote.t.Tree], parts, argsTyped, isPattern).asInstanceOf[t.Tree]
  }

}
