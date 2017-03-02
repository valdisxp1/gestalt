package scala.gestalt.dotty

import dotty.tools.dotc._
import ast.Trees._
import ast.{tpd, untpd}
import ast.untpd.modsDeco
import core.StdNames._
import core.Contexts._
import core.Symbols._
import core.Decorators._
import core.Constants._

import scala.gestalt._

object Expander {
  private object ExtractApply {
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, List[tpd.Tree], List[List[tpd.Tree]])] = tree match {
      case TypeApply(fun, targs) =>
        val Some((f, _, argss)) = unapply(fun)
        Some((f, targs, argss))
      case Apply(fun, args) =>
        val Some((f, targs, argss)) = unapply(fun)
        Some((f, targs, argss :+ args))
      case _ =>
        Some((tree, Nil, Nil))
    }
  }

  private def javaClassName(classSymbol: Symbol)(implicit ctx: Context): String = {
    val enclosingPackage = classSymbol.enclosingPackageClass
    if (enclosingPackage.isEffectiveRoot) {
      classSymbol.flatName.toString
    } else {
      enclosingPackage.showFullName + "." + classSymbol.flatName
    }
  }

  def expandQuasiquote(tree: untpd.Tree, isTerm: Boolean)(implicit ctx: Context): untpd.Tree = {
    val (tag, parts, args) = tree match {
      case Apply(Select(Apply(Ident(nme.StringContext), parts), name), args) =>
        (name.toString, parts, args)
      case UnApply(Select(Select(Apply(Select(Ident(nme.StringContext), nme.apply), List(Typed(SeqLiteral(parts, _), _))), name), nme.unapply), _, pats) =>
        (name.toString, parts, pats)
    }
    val strs = for(Literal(Constant(v: String)) <- parts) yield v
    expand(new DottyToolbox())(tag, strs, args, !isTerm)
  }

  /** Expand annotation macros */
  def expandAnnotMacro(mdef: untpd.MemberDef)(implicit ctx: Context): untpd.Tree = {
    val ann = mdef.mods.annotations.filter(macros.isAnnotMacro).headOption
    val expansion = ann.flatMap {
      case ann @ Apply(Select(New(tpt), init), _) =>
        val tpdClass = ctx.typer.typedAheadType(tpt)

        val className = tpdClass.symbol.fullName + "$inline$"
        // reflect macros definition
        val moduleClass = ctx.classloader.loadClass(className)
        val module = moduleClass.getField("MODULE$").get(null)
        val impl = moduleClass.getDeclaredMethods().find(_.getName == "apply").get
        impl.setAccessible(true)

        val expandee = {
          val mods1 = mdef.mods.withAnnotations(mdef.mods.annotations.filter(_ ne ann))
          mdef.withMods(mods1)
        }
        val result = impl.invoke(module, new DottyToolbox(), ann, expandee).asInstanceOf[untpd.Tree]
        Some(result)
      case _ =>
        None
    }

    expansion.getOrElse(mdef)
  }

  /** Expand def macros */
  def expandDefMacro(tree: tpd.Tree)(implicit ctx: Context): untpd.Tree = tree match {
    case ExtractApply(sel @ Select(prefix, method), targs, argss) =>
      val prefixType = prefix.tpe.widen
      val className = javaClassName(sel.symbol.owner) + "$inline$"
      // reflect macros definition
      val moduleClass = ctx.classloader.loadClass(className)
      val module = moduleClass.getField("MODULE$").get(null)
      val impl = moduleClass.getDeclaredMethods().find(_.getName == method.toString).get
      impl.setAccessible(true)

      val trees  = new DottyToolbox() :: prefix :: targs ++ argss.flatten
      impl.invoke(module, trees: _*).asInstanceOf[untpd.Tree]
    case _ =>
      tree
  }
}
