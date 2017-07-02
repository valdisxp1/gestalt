package scala.gestalt.dotty

import scala.gestalt.core.{ Toolbox => Tbox, Location }

import dotty.tools.dotc._
import core._
import ast.{ untpd => d, Trees => c, tpd => t }
import StdNames._
import NameOps._
import Contexts._
import Decorators._
import Constants._
import d.modsDeco
import util.Positions.Position

import scala.collection.mutable.Set


class Toolbox(enclosingPosition: Position)(implicit ctx: Context) extends Tbox {

  type Tree = d.Tree
  type TypeTree = d.Tree
  type TermTree = d.Tree
  type DefTree = d.Tree
  type PatTree = d.Tree

  type Mods = DottyModifiers

  type Splice = d.Tree
  type Ident  = d.Ident
  type Lit    = d.Literal

  type Param = d.ValDef
  type TypeParam = d.TypeDef
  type Class = d.TypeDef
  type Trait = d.TypeDef
  type Object = d.ModuleDef
  type ValDef = d.ValDef
  type ValDecl = d.ValDef
  type DefDef = d.DefDef
  type DefDecl = d.DefDef
  type Self = d.ValDef
  type InitCall = d.Tree

  /*------------------------------ positions ------------------------------*/
  type Pos = Position

  object Pos extends PosImpl {
    def pos(tree: Tree): Pos = tree.pos

    def pos(tree: tpd.Tree)(implicit c: Dummy): Pos = tree.pos
  }

  /*------------------------------ modifiers ------------------------------*/
  case class DottyModifiers(dottyMods: d.Modifiers) extends Modifiers {
    def isPrivate: Boolean = dottyMods.is(Flags.Private)
    def isProtected: Boolean = dottyMods.is(Flags.Protected)
    def isOverride: Boolean = dottyMods.is(Flags.Override)
    def isFinal: Boolean = dottyMods.is(Flags.Final)
    def isImplicit: Boolean = dottyMods.is(Flags.Implicit)
    def isLazy: Boolean = dottyMods.is(Flags.Lazy)
    def isSealed: Boolean = dottyMods.is(Flags.Sealed)
    def isAbstract: Boolean = dottyMods.is(Flags.Abstract)
    def isCase: Boolean = dottyMods.is(Flags.Case)
    def isContravariant: Boolean = dottyMods.is(Flags.Contravariant)
    def isCovariant: Boolean = dottyMods.is(Flags.Covariant)
    def isInline: Boolean = dottyMods.is(Flags.Inline)
    def isMutable: Boolean = dottyMods.is(Flags.Mutable)

    def isValParam: Boolean =
      dottyMods.is(Flags.Param) &&
        !dottyMods.is(Flags.Mutable) &&
        !dottyMods.is(Flags.allOf(Flags.Private, Flags.Local))

    def isVarParam: Boolean =
      dottyMods.is(Flags.Param) &&
        dottyMods.is(Flags.Mutable) &&
        !dottyMods.is(Flags.allOf(Flags.Private, Flags.Local))

    // can be empty or `this`
    def setPrivate(within: String): Mods =
      if (within == "this") DottyModifiers(dottyMods | Flags.Private | Flags.Local)
      else DottyModifiers(dottyMods.withPrivateWithin(within.toTypeName) | Flags.Private)

    def setProtected(within: String): Mods =
      if (within == "this") DottyModifiers(dottyMods | Flags.Protected | Flags.Local)
      else DottyModifiers(dottyMods.withPrivateWithin(within.toTypeName) | Flags.Protected)

    def setOverride: Mods = DottyModifiers(dottyMods | Flags.Override)
    def setFinal: Mods = DottyModifiers(dottyMods | Flags.Final)
    def setImplicit: Mods = DottyModifiers(dottyMods | Flags.Implicit)
    def setLazy: Mods = DottyModifiers(dottyMods | Flags.Lazy)
    def setSealed: Mods = DottyModifiers(dottyMods | Flags.Sealed)
    def setAbstract: Mods = DottyModifiers(dottyMods | Flags.Abstract)
    def setCase: Mods = DottyModifiers(dottyMods | Flags.Case)
    def setContravariant: Mods = DottyModifiers(dottyMods | Flags.Contravariant)
    def setCovariant: Mods = DottyModifiers(dottyMods | Flags.Covariant)
    def setInline: Mods = DottyModifiers(dottyMods | Flags.Inline)
    def setMutable: Mods = DottyModifiers(dottyMods | Flags.Mutable)

    def setValParam: Mods = DottyModifiers(dottyMods &~ Flags.Local &~ Flags.Mutable)
    def setVarParam: Mods = DottyModifiers(dottyMods &~ Flags.Local | Flags.Mutable)

    def withAddedAnnotation(annot: d.Tree): Mods = DottyModifiers(dottyMods.withAddedAnnotation(annot))
    def withAnnotations(annots: List[d.Tree]): Mods = DottyModifiers(dottyMods.withAnnotations(annots))

    def hasAnnotations: Boolean = dottyMods.hasAnnotations

    def annotations: List[Tree] = dottyMods.annotations

    def privateWithin: String =
      if (dottyMods.is(Flags.Local)) "this"
      else dottyMods.privateWithin.toString
  }

  // modifiers
  def emptyMods: Mods = DottyModifiers(d.EmptyModifiers)

  implicit  def toDotty(tbMods: Mods): d.Modifiers = tbMods.dottyMods

  implicit def fromDotty(dottyMods: d.Modifiers): Mods = DottyModifiers(dottyMods)

  /*------------------------------ diagnostics ------------------------------*/
  def fresh(prefix: String = "$local"): String = NameKinds.UniqueName.fresh(prefix.toTermName).toString

  // diagnostics - the implementation takes the position from the tree
  def error(message: String, pos: Pos): Unit = {
    ctx.error(message, pos)
  }

  /** stop macro transform - the implementation takes the position from the tree */
  def abort(message: String, pos: Pos): Nothing = {
    ctx.error(message, pos)
    throw new Exception(message)
  }

  /*------------------------------ helpers ------------------------------*/
  def getOrEmpty(treeOpt: Option[Tree]): Tree = treeOpt.getOrElse(d.EmptyTree)

  implicit class TreeHelper(tree: d.Tree) {
    def withPosition[T <: Tree] = tree.withPos(enclosingPosition).asInstanceOf[T]
  }

  def ApplySeq(fun: TermTree, argss: List[List[TermTree]]): TermTree =
    argss.foldLeft(fun) { (acc, args) => Apply(acc, args) }

  /*------------------------------ trees ------------------------------*/

  object NewAnonymClass extends NewAnonymClassImpl {
    def apply(parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Tree = {
      val init = d.DefDef(nme.CONSTRUCTOR, List(), List(), d.TypeTree(), d.EmptyTree)
      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      d.New(d.Template(init, parents, self, stats).withPosition)
    }
  }

  object TypeDecl extends TypeDeclImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], tboundsOpt: Option[TypeTree]): DefTree = {
      val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree))
      val body =
        if (tparams.size == 0) tbounds
        else d.LambdaTypeTree(tparams.asInstanceOf[List[d.TypeDef]], tbounds)
      d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
    }
  }

  object TypeAlias extends TypeAliasImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], rhs: TypeTree): DefTree = {
      val body =
        if (tparams.size == 0) rhs
        else d.LambdaTypeTree(tparams.asInstanceOf[List[d.TypeDef]], rhs)
      d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
    }
  }

  object PatDef extends PatDefImpl {
    def apply(mods: Mods, lhs: PatTree, tpe: Option[TypeTree], rhs: TermTree): DefTree =
      d.PatDef(mods, List(lhs), tpe.getOrElse(d.TypeTree()), rhs).withPosition
  }

  object SeqDef extends SeqDefImpl {
    def apply(mods: Mods, vals: List[String], tpe: Option[TypeTree], rhs: TermTree): DefTree =
      d.PatDef(mods, vals.map(n => d.Ident(n.toTermName)), tpe.getOrElse(d.TypeTree()), rhs).withPosition
  }

  object SeqDecl extends SeqDeclImpl {
    def apply(mods: Mods, vals: List[String], tpe: TypeTree): DefTree =
      d.PatDef(mods, vals.map(n => d.Ident(n.toTermName)), tpe, d.EmptyTree).withPosition
  }

  object SecondaryCtor extends SecondaryCtorImpl {
    def apply(mods: Mods, paramss: List[List[Param]], rhs: TermTree): DefTree =
      DefDef(mods, nme.CONSTRUCTOR.toString, Nil, paramss, Some(d.TypeTree()), rhs).withPosition
  }

  // qual.T[A, B](x, y)(z)
  object InitCall extends InitCallImpl {
    def apply(tpe: TypeTree, argss: List[List[TermTree]]): InitCall = {
      ApplySeq(tpe, argss).withPosition
    }
  }

  // new qual.T[A, B](x, y)(z)
  object NewInstance extends NewInstanceImpl {
    def apply(typeTree: TypeTree, argss: List[List[TermTree]]): TermTree = {
      ApplySeq(d.Select(d.New(typeTree), nme.CONSTRUCTOR), argss).withPosition
    }

    def apply(tp: Type, argss: List[List[tpd.Tree]])(implicit cap: Dummy): tpd.Tree = {
      argss match {
        case head :: tail => tail.foldLeft[tpd.Tree](t.New(tp, head)) { (acc, args) => Apply(acc, args) }
        case Nil => t.New(tp)
      }
    }
  }

  object Self extends SelfImpl {
    def apply(name: String, tpe: TypeTree): Self =
      d.ValDef(name.toTermName, tpe, d.EmptyTree).withPosition

    def apply(name: String): Self =
      d.ValDef(name.toTermName, d.TypeTree(), d.EmptyTree).withPosition
  }

  // types
  object TypeIdent extends TypeIdentImpl {
    def apply(name: String)(implicit c: Unsafe): TypeTree = d.Ident(name.toTypeName).withPosition
  }

  object TypeSelect extends TypeSelectImpl {
    def apply(qual: Tree, name: String): TypeTree = d.Select(qual, name.toTypeName).withPosition
  }

  object TypeSingleton extends TypeSingletonImpl {
    def apply(ref: Tree): TypeTree = d.SingletonTypeTree(ref).withPosition
  }

  object TypeApply extends TypeApplyImpl {
    def apply(tpe: TypeTree, args: List[TypeTree]): TypeTree = d.AppliedTypeTree(tpe, args).withPosition
  }

  object TypeInfix extends TypeInfixImpl {
    def apply(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree =
      d.InfixOp(lhs, d.Ident(op.toTypeName), rhs).withPosition
  }

  object TypeFunction extends TypeFunctionImpl {
    def apply(params: List[TypeTree], res: TypeTree): TypeTree =
      d.Function(params, res).withPosition
  }

  object TypeTuple extends TypeTupleImpl {
    def apply(args: List[TypeTree]): TypeTree = d.Tuple(args).withPosition
  }

  object TypeAnd extends TypeAndImpl {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree =
      d.AndTypeTree(lhs, rhs).withPosition
  }

  object TypeOr extends TypeOrImpl {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree =
      d.OrTypeTree(lhs, rhs).withPosition
  }

  object TypeRefine extends TypeRefineImpl {
    def apply(stats: List[Tree]): TypeTree =
      d.RefinedTypeTree(d.EmptyTree, stats).withPosition

    def apply(tpe: TypeTree, stats: List[Tree]): TypeTree =
      d.RefinedTypeTree(tpe, stats).withPosition
  }

  object TypeBounds extends TypeBoundsImpl {
    def apply(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree = {
      require(lo.nonEmpty || hi.nonEmpty)
      d.TypeBoundsTree(lo.getOrElse(d.EmptyTree), hi.getOrElse(d.EmptyTree)).withPosition
    }
  }

  object TypeRepeated extends TypeRepeatedImpl {
    def apply(tpe: TypeTree): TypeTree =
      d.PostfixOp(tpe, d.Ident(nme.raw.STAR)).withPosition
  }

  object TypeByName extends TypeByNameImpl {
    def apply(tpe: TypeTree): TypeTree =
      d.ByNameTypeTree(tpe).withPosition
  }

  object TypeAnnotated extends TypeAnnotatedImpl {
    def apply(tpe: TypeTree, annots: List[Tree]): TypeTree = {
      require(annots.size > 0)
      annots.tail.foldRight(d.Annotated(tpe, annots.head).withPosition) { (ann: Tree, acc: TypeTree) =>
        d.Annotated(acc, ann).withPosition[TypeTree]
      }
    }
  }

  // terms

  object Super extends SuperImpl {
    def apply(thisp: String, superp: String): TermTree =
      d.Super(d.Ident(thisp.toTypeName), d.Ident(superp.toTypeName)).withPosition
  }

  object Interpolate extends InterpolateImpl {
    def apply(prefix: String, parts: List[String], args: List[TermTree]): TermTree = {
      val thickets =
        for {(arg, part) <- args.zip(parts.take(args.size))}
          yield {
            val expr = arg match {
              case tree: d.Ident => tree
              case tree => d.Block(Nil, tree)
            }
            d.Thicket(Lit(part), expr)
          }
      val segments =
        if (parts.size > args.size)
          thickets :+ Lit(parts.last)
        else thickets

      d.InterpolatedString(prefix.toTermName, segments).withPosition
    }
  }


  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  object Infix extends InfixImpl {
    def apply(lhs: TermTree, op: String, rhs: TermTree): TermTree =
      d.InfixOp(lhs, d.Ident(op.toTermName), rhs).withPosition
  }

  object Prefix extends PrefixImpl {
    def apply(op: String, od: TermTree): TermTree = d.PrefixOp(d.Ident(op.toTermName), od).withPosition
  }

  object Postfix extends PostfixImpl {
    def apply(od: TermTree, op: String): TermTree =
      d.PostfixOp(od, d.Ident(op.toTermName)).withPosition
  }

  object Return extends ReturnImpl {
    def apply(expr: TermTree): TermTree = d.Return(expr, d.EmptyTree).withPosition
    def apply: TermTree = d.Return(d.EmptyTree, d.EmptyTree).withPosition
    def unapply(tree: Tree): Option[Option[TermTree]] = tree match {
      case c.Return(expr, _) => Some(if (expr.isEmpty) None else Some(expr))
      case _ => None
    }
    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[tpd.Tree] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[tpd.Tree]]
  }

  object Throw extends ThrowImpl {
    def apply(expr: TermTree): TermTree = d.Throw(expr).withPosition
  }

  object If extends IfImpl {
    def apply(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree =
      d.If(cond, thenp, elsep.getOrElse(d.EmptyTree)).withPosition

    def unapply(tree: Tree): Option[(TermTree, TermTree, Option[TermTree])] = tree match {
      case c.If(cond, thenp, elsep) =>
        Some(cond, thenp, if (elsep.isEmpty) None else Some(elsep))
      case _ => None
    }

    def apply(cond: tpd.Tree, thenp: tpd.Tree, elsep: tpd.Tree)(implicit c: Dummy): tpd.Tree =
      t.If(cond, thenp, elsep)

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[(tpd.Tree, tpd.Tree, tpd.Tree)] = tree match {
      case tree: t.If => Some(tree.cond, tree.thenp, tree.elsep)
      case _          => None
    }
  }

  object Try extends TryImpl {
    def apply(expr: TermTree, cases: List[Tree], finallyp: Option[TermTree]): TermTree =
      d.Try(expr, cases.asInstanceOf[List[d.CaseDef]], finallyp.getOrElse(d.EmptyTree)).withPosition

    def apply(expr: TermTree, handler: Tree, finallyp: Option[TermTree]): TermTree =
      d.ParsedTry(expr, handler, finallyp.getOrElse(d.EmptyTree)).withPosition
  }

  object Function extends FunctionImpl {
    def apply(params: List[Param], body: TermTree): TermTree =
      d.Function(params, body).withPosition

    def apply(params: List[(String, Type)], resTp: Type)(bodyFn: List[tpd.Tree] => tpd.Tree): tpd.Tree = {
      val meth = ctx.newSymbol(
        ctx.owner, nme.ANON_FUN,
        Flags.Synthetic | Flags.Method,
        Types.MethodType(params.map(_._1.toTermName), params.map(_._2), resTp)
      )
      t.Closure(meth, paramss => {
        ensureOwner(bodyFn(paramss.head), meth)
      })
    }


    def unapply(tree: tpd.Tree): Option[(List[Symbol], tpd.Tree)] = tree match {
      case c.Block((meth : t.DefDef) :: Nil, _ : t.Closure) if meth.name == nme.ANON_FUN =>
        Some((meth.vparamss.head.map(_.symbol), meth.rhs))
      case _ => None
    }
  }

  object While extends WhileImpl {
    def apply(expr: TermTree, body: TermTree): TermTree = d.WhileDo(expr, body).withPosition
  }

  object DoWhile extends DoWhileImpl {
    def apply(body: TermTree, expr: TermTree): TermTree = d.DoWhile(body, expr).withPosition
  }

  object For extends ForImpl {
    def ForDo(enums: List[Tree], body: TermTree): TermTree = d.ForDo(enums, body)
    def ForYield(enums: List[Tree], body: TermTree): TermTree = d.ForYield(enums, body)
    def GenFrom(pat: PatTree, rhs: TermTree): Tree = d.GenFrom(pat, rhs)
    def GenAlias(pat: PatTree, rhs: TermTree): Tree = d.GenAlias(pat, rhs)
    def Guard(cond: TermTree): Tree = cond
  }

  object Named extends NamedImpl {
    def apply(name: String, expr: TermTree): TermTree =
      d.NamedArg(name.toTermName, expr).withPosition
  }

  object Repeated extends RepeatedImpl {
    def apply(expr: TermTree): TermTree =
      d.Typed(expr, d.Ident(tpnme.WILDCARD_STAR)).withPosition
  }

  // patterns
  object Pat extends PatImpl {
    def Bind(name: String, expr: PatTree): PatTree =
      d.Bind(name.toTermName, expr).withPosition

    def Alt(trees: List[PatTree]): PatTree =
      d.Alternative(trees).withPosition

    def Var(name: String): PatTree =
      d.Ident(name.toTermName).withPosition

    def Ascribe(name: String, tp: TypeTree): PatTree =
      d.Typed(d.Ident(name.toTermName), tp).withPosition

    def Unapply(fun: TermTree, args: List[PatTree]): PatTree =
      d.Apply(fun, args).withPosition

    def Infix(lhs: PatTree, op: String, rhs: PatTree): PatTree =
      d.InfixOp(lhs, d.Ident(op.toTermName), rhs).withPosition

    def Tuple(pats: List[PatTree]): PatTree =
      d.Tuple(pats)
  }

  // importees
  object Import extends ImportImpl {
    def apply(items: List[Tree]): Tree =
      if (items.size == 1)
        items.head.withPosition
      else
        d.Thicket(items).withPosition

    def Item(ref: Tree, importees: List[Tree]): Tree =
      d.Import(ref, importees).withPosition

    def Name(name: String): Tree = d.Ident(name.toTermName).withPosition

    def Rename(from: String, to: String): Tree =
      d.Thicket(d.Ident(from.toTermName), d.Ident(to.toTermName)).withPosition

    def Hide(name: String): Tree =
      d.Thicket(d.Ident(name.toTermName), d.Ident(nme.WILDCARD)).withPosition
  }

  // extractors
  object Lit extends LitImpl {
    def apply(value: Any): Lit = d.Literal(Constant(value)).withPosition
    def unapply(tree: Tree): Option[Any] = tree match {
      case c.Literal(Constant(v)) => Some(v)
      case _ => None
    }

    def typed(value: Any): tpd.Tree = t.Literal(Constant(value)).withPosition

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[Any] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[Any]]
  }

  object Ident extends IdentImpl {
    def apply(name: String)(implicit c: Unsafe): Ident = d.Ident(name.toTermName).withPosition
    def unapply(tree: Tree): Option[String] = tree match {
      case c.Ident(name) if name.isTermName => Some(name.show)
      case _ => None
    }

    def apply(symbol: Symbol): tpd.Tree = t.ref(symbol)
    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[String] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[String]]
  }

  object This extends ThisImpl {
    def apply(qual: String): TermTree = d.This(d.Ident(qual.toTypeName)).withPosition
    def unapply(tree: Tree): Option[String] = tree match {
      case c.This(c.Ident(name)) => Some(name.show)
      case _ => None
    }

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[String] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[String]]
  }

  object Select extends SelectImpl {
    def apply(qual: TermTree, name: String): TermTree = d.Select(qual, name.toTermName).withPosition
    def unapply(tree: Tree): Option[(TermTree, String)] = tree match {
      case c.Select(qual, name) if name.isTermName => Some((qual, name.show))
      case _ => None
    }

    def apply(qual: tpd.Tree, name: String)(implicit c: Dummy): tpd.Tree =
      t.Select(qual, name.toTermName)
      // t.Select(qual.withTypeUnchecked(qual.tpe.widen), name.toTermName)

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[(tpd.Tree, String)] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[(tpd.Tree, String)]]
  }

  object Apply extends ApplyImpl {
    def apply(fun: TermTree, args: List[TermTree]): TermTree = d.Apply(fun, args).withPosition

    def unapply(tree: Tree): Option[(TermTree, List[TermTree])] = tree match {
      case c.Apply(fun, Seq(c.Typed(c.SeqLiteral(args, _), _))) => Some((fun, args))
      case c.Apply(fun, args) => Some((fun, args))
      case _ => None
    }

    def apply(fun: tpd.Tree, args: List[tpd.Tree])(implicit c: Dummy): tpd.Tree = t.Apply(fun, args)

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[(tpd.Tree, List[tpd.Tree])] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[(tpd.Tree, List[tpd.Tree])]]
  }

  object Ascribe extends AscribeImpl {
    def apply(expr: TermTree, tpe: TypeTree): TermTree = d.Typed(expr, tpe).withPosition

    def unapply(tree: Tree): Option[(TermTree, TypeTree)] = tree match {
      case c.Typed(expr, tpe) => Some((expr, tpe))
      case _ => None
    }

    def apply(expr: tpd.Tree, tpe: tpd.Tree)(implicit c: Dummy): tpd.Tree =
      t.Typed(expr, tpe)

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[(tpd.Tree, tpd.Tree)] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[(tpd.Tree, tpd.Tree)]]
  }

  object Assign extends AssignImpl {
    def apply(lhs: TermTree, rhs: TermTree): TermTree = d.Assign(lhs, rhs).withPosition

    def unapply(tree: Tree): Option[(TermTree, TermTree)] = tree match {
      case c.Assign(lhs, rhs) => Some((lhs, rhs))
      case _ => None
    }

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[(tpd.Tree, tpd.Tree)] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[(tpd.Tree, tpd.Tree)]]
  }

  object Update extends UpdateImpl {
    def apply(fun: TermTree, argss: List[List[TermTree]], rhs: TermTree): TermTree = {
      d.Assign(ApplySeq(fun, argss), rhs)
    }
  }


  object Annotated extends AnnotatedImpl {
    def apply(expr: TermTree, annots: List[Tree]): TermTree = {
      require(annots.size > 0)
      annots.tail.foldRight(d.Annotated(expr, annots.head).withPosition) { (ann: Tree, acc: TermTree) =>
        d.Annotated(acc, ann).withPosition[TermTree]
      }
    }

    def unapply(tree: Tree): Option[(TermTree, List[Tree])] = {
      def recur(term: Tree, acc: List[Tree]): (Tree, List[Tree])  = term match {
        case c.Annotated(expr, annot) => recur(expr, annot +: acc) // inner-most is in the front
        case expr => (expr, acc)
      }
      val (expr, annots) = recur(tree, Nil)
      if (annots.size > 0) Some((expr, annots))
      else None
    }
  }

  object Block extends BlockImpl {
    def apply(stats: List[Tree]): TermTree = {
      if (stats.size == 0)
        d.Block(stats, d.EmptyTree).withPosition
      else
        d.Block(stats.init, stats.last).withPosition
    }

    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case c.Block(stats, expr) => Some(stats :+ expr)
      case _ => None
    }

    def apply(stats: List[tpd.Tree], expr: tpd.Tree)(implicit c: Dummy): tpd.Tree =
      t.Block(stats, expr)

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[(List[tpd.Tree], tpd.Tree)] = tree match {
      case block: t.Block => Some((block.stats, block.expr))
      case _ => None
    }
  }

  object Match extends MatchImpl {
    def apply(expr: TermTree, cases: List[Tree]): TermTree =
      d.Match(expr, cases.asInstanceOf[List[d.CaseDef]]).withPosition

    def unapply(tree: Tree): Option[(TermTree, List[Tree])] = tree match {
      case c.Match(expr, cases) => Some((expr, cases))
      case _ => None
    }

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[(tpd.Tree, List[tpd.Tree])] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[(tpd.Tree, List[tpd.Tree])]]
  }

  object Case extends CaseImpl {
    def apply(pat: PatTree, cond: Option[TermTree], body: TermTree): Tree =
      d.CaseDef(pat, cond.getOrElse(d.EmptyTree), body).withPosition

    def unapply(tree: Tree): Option[(TermTree, Option[TermTree], TermTree)] = tree match {
      case c.CaseDef(pat, cond, body) =>
        val condOpt = if (cond == d.EmptyTree) None else Some(cond)
        Some((pat, condOpt, body))
      case _ => None
    }

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[(tpd.Tree, Option[tpd.Tree], tpd.Tree)] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[(tpd.Tree, Option[tpd.Tree], tpd.Tree)]]
  }

  object PartialFunction extends PartialFunctionImpl {
    def apply(cases: List[Tree]): TermTree =
      d.Match(d.EmptyTree, cases.asInstanceOf[List[d.CaseDef]]).withPosition

    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case c.Match(d.EmptyTree, cases) => Some(cases)
      case _ => None
    }
  }

  object Tuple extends TupleImpl {
    def apply(args: List[TermTree]): TermTree = d.Tuple(args).withPosition
    def apply(ts: List[tpd.Tree])(implicit c: Dummy): tpd.Tree = {
        val arity = ts.length
        def tupleTypeRef: TypeRef = Type.typeRef("scala.Tuple"+arity)
        if (arity > Definitions.MaxTupleArity) {
          error(s"Cannot create a tuple with $arity elements, maximum: ${Definitions.MaxTupleArity} (MaxTupleArity)", enclosingPosition)
          val unit = ()
          t.Literal(Constant(unit))
        } else if (arity == 1) ts.head
        else t.AppliedTypeTree(t.TypeTree(tupleTypeRef), ts).withPosition
    }

    def unapply(tree: Tree): Option[List[TermTree]] = tree match {
      case d.Tuple(trees) => Some(trees)
      case _ => None
    }

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[List[tpd.Tree]] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[List[tpd.Tree]]]
  }


  object SeqLiteral extends SeqLiteralImpl {
    def apply(trees: List[tpd.Tree], tp: Type): tpd.Tree = {
      val tpSeq = ctx.definitions.RepeatedParamType.appliedTo(tp)
      t.Typed(t.SeqLiteral(trees, t.TypeTree(tp)), t.TypeTree(tpSeq))
    }

    def unapply(tree: tpd.Tree): Option[List[tpd.Tree]] = tree match {
      case c.Typed(c.SeqLiteral(elems,_), _) => Some(elems)
      case _ => None
    }
  }

  object ApplyType extends ApplyTypeImpl {
    def apply(fun: Tree, args: List[TypeTree]): TermTree = d.TypeApply(fun, args).withPosition
    def unapply(tree: Tree): Option[(TermTree, List[TypeTree])] = tree match {
      case c.TypeApply(fun, args) => Some((fun, args))
      case _ => Some((tree, Nil))
    }

    def apply(fun: tpd.Tree, args: List[tpd.Tree])(implicit c: Dummy): tpd.Tree =
      t.TypeApply(fun, args)

    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[(tpd.Tree, List[tpd.Tree])] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[(tpd.Tree, List[tpd.Tree])]]
  }


  object Object extends ObjectImpl {
    def apply(mods: Mods, name: String, parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Object = {
      val constr = d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      val templ = d.Template(constr, parents, self, stats)
      d.ModuleDef(name.toTermName, templ).withMods(mods).withPosition
    }

    def unapply(tree: Tree): Option[(Mods, String, List[InitCall], Option[Self], List[Tree])] = tree match {
      case obj @ d.ModuleDef(name, templ @ c.Template(constr, parents, self: d.ValDef, body)) =>
        val selfOpt = if (self == d.EmptyValDef) None else Some(Self(self.name.toString, self.tpt))
        Some((mods(obj), name.toString, parents, selfOpt, templ.body))
      case _ => None
    }

    def mods(tree: Object): Mods = new modsDeco(tree).mods
    def name(tree: Object): String = tree.name.toString
    def parents(tree: Object): List[InitCall] = tree match {
      case d.ModuleDef(_, c.Template(_, parents, _, _)) => parents
    }

    def selfOpt(tree: Object): Option[Self] = tree match {
      case d.ModuleDef(_, c.Template(_, _, self, _)) => if (self.isEmpty) None else Some(self)
    }

    def stats(tree: Object): List[Tree] = tree match {
      case d.ModuleDef(_, tmpl @ c.Template(_, _, _, _)) => tmpl.forceIfLazy
    }

    def copyStats(tree: Object)(stats: List[Tree]) = {
      val tmpl = d.cpy.Template(tree.impl)(body = stats)
      d.cpy.ModuleDef(tree)(name = tree.name, impl = tmpl)
    }

    def get(tree: Tree): Option[Object] = tree match {
      case obj @ d.ModuleDef(_, c.Template(self, _, _, _)) => Some(obj)
      case _ => None
    }
  }

  object Class extends ClassImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Class = {
      val constr =
        if (paramss.isEmpty) d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
        else {
          d.DefDef(nme.CONSTRUCTOR, tparams, paramss, d.TypeTree(), d.EmptyTree).withMods(ctorMods)
        }

      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      val templ = d.Template(constr, parents, self, stats)
      d.TypeDef(name.toTypeName, templ).withMods(mods).withPosition
    }

    def unapply(tree: Tree): Option[(Mods, String, List[TypeParam], Mods, List[List[Param]], List[InitCall], Option[Self], List[Tree])] = tree match {
      case cdef @ c.TypeDef(name, templ @ c.Template(constr, parents, self, body)) =>
        var tparams: List[TypeParam] = Nil
        val (paramss, cmods) = constr match {
          case c.DefDef(nme.CONSTRUCTOR, Nil, Nil, c.TypeTree(), d.EmptyTree) => (Nil, emptyMods)
          case pctor : d.DefDef =>
            tparams = pctor.tparams
            (pctor.vparamss, (new modsDeco(pctor).mods): Mods)
        }
        val selfOpt = if (self == d.EmptyValDef) None else Some(self)
        Some((mods(cdef), name.toString, tparams, cmods, paramss, parents, selfOpt, templ.body))
      case _ => None
    }

    def mods(tree: Class): Mods = new modsDeco(tree).mods
    def name(tree: Class): String = tree.name.toString
    def parents(tree: Class): List[InitCall] = tree match {
      case c.TypeDef(_, c.Template(_, parents, _, _)) => parents
    }

    def ctorMods(tree: Class): Mods = tree match {
      case c.TypeDef(_, c.Template(ctor, _, _, _)) => new modsDeco(ctor).mods
    }

    def paramss(tree: Class): List[List[Param]] = tree match {
      case c.TypeDef(_, c.Template(ctor, _, _, _)) => ctor.vparamss
    }

    def tparams(tree: Class): List[TypeParam] = tree match {
      case c.TypeDef(_, c.Template(ctor, _, _, _)) => ctor.tparams
    }

    def selfOpt(tree: Class): Option[Self] = tree match {
      case c.TypeDef(_, c.Template(_, _, self, _)) => if (self.isEmpty) None else Some(self)
    }

    def stats(tree: Class): List[Tree] = tree match {
      case c.TypeDef(_, tmpl : d.Template) => tmpl.forceIfLazy
    }

    def copyMods(tree: Class)(mods: Mods) = tree.withMods(mods)

    def copyParamss(tree: Class)(paramss: List[List[Param]]) = {
      val tmpl1 = tree.rhs.asInstanceOf[d.Template]
      val contr2 = d.cpy.DefDef(tmpl1.constr)(vparamss = paramss)
      val tmpl2 = d.cpy.Template(tmpl1)(constr = contr2)
      d.cpy.TypeDef(tree)(rhs = tmpl1)
    }

    def copyStats(tree: Class)(stats: List[Tree]) = {
      val tmpl1 = tree.rhs.asInstanceOf[d.Template]
      val tmpl2 = d.cpy.Template(tmpl1)(body = stats)
      d.cpy.TypeDef(tree)(rhs = tmpl2)
    }

    def get(tree: Tree): Option[Class] = tree match {
      case cdef @ c.TypeDef(_, _ : d.Template) if !mods(cdef).is(Flags.Trait) => Some(cdef)
      case _ => None
    }
  }

  object Trait extends TraitImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], self: Option[Self], stats: List[Tree]): Trait =
      Class(mods.dottyMods | Flags.Trait, name, tparams, ctorMods, paramss, parents, self, stats).withPosition

    def unapply(tree: Tree): Option[(Mods, String, List[TypeParam], Mods, List[List[Param]], List[InitCall], Option[Self], List[Tree])] =
      if (!tree.isInstanceOf[d.TypeDef]) return None
      else {
        val typedef = tree.asInstanceOf[d.TypeDef]
        if (!typedef.mods.is(Flags.Trait)) return None

        Class.unapply(typedef)
      }

    def mods(tree: Trait): Mods = new modsDeco(tree).mods
    def name(tree: Trait): String = tree.name.toString
    def parents(tree: Trait): List[InitCall] = Trait.parents(tree)

    def paramss(tree: Trait): List[List[Param]] = Class.paramss(tree)

    def tparams(tree: Trait): List[TypeParam] = Class.tparams(tree)

    def selfOpt(tree: Trait): Option[Self] = Class.selfOpt(tree)

    def stats(tree: Trait): List[Tree] = Class.stats(tree)

    def copyStats(tree: Trait)(stats: List[Tree]) = Class.copyStats(tree)(stats)

    def get(tree: Tree): Option[Trait] = tree match {
      case cdef @ c.TypeDef(_, _ : d.Template) if mods(cdef).is(Flags.Trait)  => Some(cdef)
      case _ => None
    }
  }

   // accessors
  object Param extends ParamImpl {
    def apply(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[TermTree]): Param = {
      d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), getOrEmpty(default))
        .withMods(mods.dottyMods | Flags.TermParam).withPosition.asInstanceOf[Param]
    }

    def mods(tree: Param): Mods = new modsDeco(tree).mods
    def name(tree: Param): String = tree.name.show
    def tptOpt(tree: Param): Option[TypeTree] = if (tree.tpt.isInstanceOf[d.TypeTree]) None else Some(tree.tpt)
    def defaultOpt(tree: Param): Option[TermTree] = if (tree.forceIfLazy == d.EmptyTree) None else Some(tree.forceIfLazy)
    def copyMods(tree: Param)(mods: Mods): Param = tree.withMods(mods)

    def symbol(tree: tpd.Param)(implicit c: Dummy): Symbol = tree.symbol
    def name(tree: tpd.Param)(implicit c: Dummy): String = tree.name.show
    def tpt(tree: tpd.Param)(implicit c: Dummy): t.Tree = tree.tpt
  }

  object TypeParam extends TypeParamImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], tboundsOpt: Option[TypeTree], cbounds: List[TypeTree]): TypeParam = {
      val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree)).asInstanceOf[d.TypeBoundsTree]
      val inner =
        if (cbounds.size == 0) tbounds
        else d.ContextBounds(tbounds, cbounds.map(d.AppliedTypeTree(_, d.Ident(name.toTypeName))))

      val body =
        if (tparams.size == 0) inner
        else d.LambdaTypeTree(tparams, inner)

      d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
    }

    def mods(tree: TypeParam): Mods = new modsDeco(tree).mods
    def name(tree: TypeParam): String = tree.name.show
    def tparams(tree: TypeParam): List[TypeParam] = tree match {
      case c.TypeDef(_, c.LambdaTypeTree(tparams, _)) => tparams
      case c.TypeDef(_, _) => Nil // bounds
    }
  }

  object ValDef extends ValDefImpl {
    def apply(mods: Mods, name: String, tpe: Option[TypeTree], rhs: TermTree): ValDef =
      d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition

    def mods(tree: ValDef): Mods = new modsDeco(tree).mods
    def name(tree: ValDef): String = tree.name.show
    def rhs(tree: ValDef): TermTree = tree.forceIfLazy
    def tptOpt(tree: ValDef): Option[TypeTree] = if (tree.tpt.isInstanceOf[d.TypeTree]) None else Some(tree.tpt)
    def copyRhs(tree: ValDef)(rhs: TermTree): ValDef = d.cpy.ValDef(tree)(rhs = rhs)

    def get(tree: Tree): Option[ValDef] = tree match {
      case vdef : d.ValDef if !vdef.forceIfLazy.isEmpty => Some(vdef)
      case _ => None
    }

    def unapply(tree: Tree): Option[(String, Option[TypeTree], TermTree)] = tree match {
      case vdef : d.ValDef if !vdef.forceIfLazy.isEmpty =>
        Some((name(vdef), tptOpt(vdef), rhs(vdef)))
      case _ => None
    }

    def apply(name: String, rhs: tpd.Tree): tpd.ValDef = {
      val vsym = ctx.newSymbol(ctx.owner, name.toTermName, Flags.EmptyFlags, rhs.tpe)
      t.ValDef(vsym, rhs)
    }

    def symbol(tree: tpd.ValDef)(implicit c: Dummy): Symbol = tree.symbol
    def name(tree: tpd.ValDef)(implicit c: Dummy): String = tree.name.show
    def rhs(tree: tpd.ValDef)(implicit c: Dummy): TermTree = tree.forceIfLazy
    def tptOpt(tree: tpd.ValDef)(implicit c: Dummy): Option[TypeTree] = Some(tree.tpt)
    def copyRhs(tree: tpd.ValDef)(rhs: tpd.Tree)(implicit c: Dummy): tpd.ValDef = t.cpy.ValDef(tree)(rhs = rhs)
    def get(tree: tpd.Tree)(implicit c: Dummy): Option[tpd.ValDef] =
      get(tree.asInstanceOf[Tree]).asInstanceOf[Option[tpd.ValDef]]
    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[(String, Option[TypeTree], TermTree)] =
      unapply(tree.asInstanceOf[Tree]).asInstanceOf[Option[(String, Option[TypeTree], TermTree)]]
  }

  object ValDecl extends ValDeclImpl {
    def apply(mods: Mods, name: String, tpe: TypeTree): ValDecl =
      d.ValDef(name.toTermName, tpe, d.EmptyTree).withMods(mods).withPosition

    def mods(tree: ValDecl): Mods = new modsDeco(tree).mods
    def name(tree: ValDecl): String = tree.name.show
    def tpt(tree: ValDecl): TypeTree = tree.tpt

    def get(tree: Tree): Option[ValDecl] = tree match {
      case vdef : d.ValDef if vdef.forceIfLazy.isEmpty  => Some(vdef)
      case _ => None
    }

    def unapply(tree: Tree): Option[(Mods, String, TypeTree)] = tree match {
      case vdef : d.ValDef if vdef.forceIfLazy.isEmpty  =>
        Some((mods(vdef), name(vdef), vdef.tpt))
      case _ => None
    }
  }

  object DefDef extends DefDefImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: Option[TypeTree], rhs: TermTree): DefDef = {
      d.DefDef(name.toTermName, tparams, paramss, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition
    }

    def mods(tree: DefDef): Mods = new modsDeco(tree).mods
    def name(tree: DefDef): String = tree.name.show
    def tptOpt(tree: DefDef): Option[TypeTree] = if (tree.tpt.isInstanceOf[d.TypeTree]) None else Some(tree.tpt)
    def tparams(tree: DefDef): List[TypeParam] = tree.tparams
    def paramss(tree: DefDef): List[List[Param]] = tree.vparamss
    def rhs(tree: DefDef): TermTree = tree.forceIfLazy
    def copyRhs(tree: DefDef)(rhs: TermTree): DefDef = d.cpy.DefDef(tree)(rhs = rhs)

    def get(tree: Tree): Option[DefDef] = tree match {
      case ddef : d.DefDef if !ddef.forceIfLazy.isEmpty  => Some(ddef)
      case _ => None
    }

    def unapply(tree: Tree): Option[(Mods, String, List[TypeParam], List[List[Param]], Option[TypeTree], TermTree)] = tree match {
      case ddef : d.DefDef if !ddef.forceIfLazy.isEmpty  =>
        Some((mods(ddef), name(ddef), ddef.tparams, ddef.vparamss, tptOpt(ddef), rhs(ddef)))
      case _ => None
    }
  }

  object DefDecl extends DefDeclImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: TypeTree): DefDecl = {
      d.DefDef(name.toTermName, tparams, paramss, tpe, d.EmptyTree).withMods(mods).withPosition
    }

    def get(tree: Tree): Option[DefDecl] = tree match {
      case ddef : d.DefDef if ddef.forceIfLazy.isEmpty => Some(ddef)
      case _ => None
    }

    def unapply(tree: Tree): Option[(Mods, String, List[TypeParam], List[List[Param]], TypeTree)] = tree match {
      case ddef : d.DefDef if ddef.forceIfLazy.isEmpty =>
        Some((mods(ddef), name(ddef), ddef.tparams, ddef.vparamss, ddef.tpt))
      case _ => None
    }

    def mods(tree: DefDecl): Mods = new modsDeco(tree).mods
    def name(tree: DefDecl): String = tree.name.show
    def tparams(tree: DefDecl): List[TypeParam] = tree.tparams
    def paramss(tree: DefDecl): List[List[Param]] = tree.vparamss
    def tpt(tree: DefDecl): TypeTree = tree.tpt
  }

  object TypedSplice extends TypedSpliceImpl {
    def apply(tree: tpd.Tree): Splice = {
      val owners = getOwners(tree)

      // make sure the spliced tree only has one owner
      val treeNew = if (owners.length > 1) ensureOwner(tree, owners.head) else tree

      val newCtx = if (owners.isEmpty) ctx else ctx.withOwner(owners.head)
      d.TypedSplice(treeNew)(newCtx)
    }

    def unapply(tree: Tree): Option[tpd.Tree] = tree match {
      case d.TypedSplice(tree) => Some(tree)
      case _ => None
    }
  }


  /*------------------------------- TreeOps-------------------------------------*/

  object untpd extends untpdImpl {
    def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit =
      new d.UntypedTreeMap() {
        override def transform(tree: Tree)(implicit ctx: Context) = {
          pf.lift(tree).getOrElse(super.transform(tree))
          tree
        }
      }.transform(tree)

    def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean = {
      var r = false
      traverse(tree) {
        case t if pf.isDefinedAt(t) && !r => r = pf(t)
      }
      r
    }

    def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree = {
      new d.UntypedTreeMap() {
        override def transform(tree: Tree)(implicit ctx: Context) =
          pf.lift(tree).getOrElse(super.transform(tree))
      }.transform(tree)
    }
  }


  object tpd extends tpdImpl {
    type Tree    = t.Tree
    type ValDef  = t.ValDef
    type Param   = t.ValDef

    def traverse(tree: tpd.Tree)(pf: PartialFunction[tpd.Tree, Unit]): Unit =
      new t.TreeTraverser {
        override def traverse(tree: tpd.Tree)(implicit ctx: Context) =
          pf.lift(tree).getOrElse(super.traverseChildren(tree))
      }.traverse(tree)

    def exists(tree: tpd.Tree)(pf: PartialFunction[tpd.Tree, Boolean]): Boolean = {
      var r = false
      traverse(tree) {
        case t if pf.isDefinedAt(t) && !r => r = pf(t)
      }
      r
    }

    def transform(tree: tpd.Tree)(pf: PartialFunction[tpd.Tree, tpd.Tree]): tpd.Tree = {
      new t.TreeMap() {
        override def transform(tree: tpd.Tree)(implicit ctx: Context) =
          pf.lift(tree).getOrElse(super.transform(tree))
      }.transform(tree)
    }

    /** subst symbols in tree */
    def subst(tree: tpd.Tree)(from: List[Symbol], to: List[Symbol]): tpd.Tree = new t.TreeOps(tree).subst(from, to)

    /** type associated with the tree */
    def typeOf(tree: tpd.Tree): Type = tree.tpe
  }

  /*------------------------------- types -------------------------------------*/

  type Type       = Types.Type
  type TermRef    = Types.TermRef
  type TypeRef    = Types.TypeRef
  type MethodType = Types.MethodType
  type Symbol     = Symbols.Symbol

  /** get the location where the def macro is used */
  def location: Location = Location(ctx.compilationUnit.source.file.name, enclosingPosition.line(), enclosingPosition.column())

  object Type extends TypeImpl {

    /** pretty print type */
    def show(tp: Type): String = tp.show

    /** are the two types equal? */
    def =:=(tp1: Type, tp2: Type): Boolean = tp1 =:= tp2

    /** is `tp1` a subtype of `tp2` */
    def <:<(tp1: Type, tp2: Type): Boolean = tp1 <:< tp2

    def lub(tp1: Type, tp2: Type): Type = ctx.typeComparer.lub(tp1, tp2, false)

    /** returning a type referring to a type definition */
    def typeRef(path: String): TypeRef = ctx.staticRef(path.toTypeName, false).symbol.typeRef

    /** returning a type referring to a term definition */
    def termRef(path: String): TermRef = ctx.staticRef(path.toTermName, false).symbol.termRef

    def isCaseClass(tp: Type): Boolean = tp.classSymbol.is(Flags.Case)

    /** class symbol associated with the type */
    def classSymbol(tp: Type): Option[Symbol] = tp.classSymbol match {
      case Symbols.NoSymbol => None
      case cls      => Some(cls)
    }

    /** val fields of a case class Type -- only the ones declared in primary constructor */
    def caseFields(tp: Type): List[Denotation] = {
      tp.memberDenots(
        Types.fieldFilter,
        (name, buf) => buf ++= tp.member(name).altsWith(_ is Flags.ParamAccessor)
      ).toList
    }

    /* field with the given name */
    def fieldIn(tp: Type, name: String): Option[Denotation] = {
      tp.memberExcluding(name.toTermName, Flags.Method).altsWith(
        p => p.owner == tp.widenSingleton.classSymbol
      ).headOption
    }

    def fieldsIn(tp: Type): List[Denotation] = {
      tp.memberDenots(
        Types.fieldFilter,
        (name, buf) => buf ++= tp.member(name).altsWith(
          p => !p.is(Flags.Method) && p.owner == tp.widenSingleton.classSymbol
        )
      ).toList
    }

    def method(tp: Type, name: String): List[Denotation] = {
      tp.member(name.toTermName).altsWith(p => p.is(Flags.Method))
    }

    def methods(tp: Type): List[Denotation] = {
      tp.memberDenots(
        Types.takeAllFilter,
        (name, buf) => buf ++= tp.member(name).altsWith(p => p.is(Flags.Method) && !p.isConstructor)
      ).toList
    }

    def methodIn(tp: Type, name: String): List[Denotation] = {
      tp.member(name.toTermName).altsWith(
        p => p.is(Flags.Method) && p.owner == tp.widenSingleton.classSymbol
      )
    }

    def methodsIn(tp: Type): List[Denotation] = {
      tp.memberDenots(
        Types.takeAllFilter,
        (name, buf) => buf ++= tp.member(name).altsWith(
          p => p.is(Flags.Method) && p.owner == tp.widenSingleton.classSymbol && !p.isConstructor
        )
      ).toList
    }

    def companion(tp: Type): Option[Type] = {
      val clazz = tp.widenSingleton.classSymbol
      if (clazz.exists)
        if (clazz.is(Flags.Module) && clazz.companionClass.exists)
          Some(clazz.companionClass.namedType)
        else if (!clazz.is(Flags.Module) && clazz.companionModule.exists)
          Some(clazz.companionModule.namedType)
        else None
      else None
    }

    def widen(tp: Type): Type = tp.widen

    def denot(tp: Type): Option[Denotation] = tp match {
      case tp: Types.NamedType => Some(tp.denot)
      case tp: Types.TypeProxy => denot(tp.underlying)
      case _ => None
    }

    /** The type representing  T[U1, ..., Un] */
    def appliedTo(tp: Type, args: List[Type]): Type = new TypeApplications(tp).appliedTo(args)(ctx)

    def toTree(tp: Type): tpd.Tree = t.TypeTree(tp)

    /** Infer an implicit instance of the given type */
    def infer(tp: Type): Option[tpd.Tree] = {
      var hasError = false
      def implicitArgError(msg: String => String) = hasError = true

      val res = ctx.typer.inferImplicitArg(tp, implicitArgError, enclosingPosition)
      if (hasError) None
      else Some(res)
    }
  }

  object ByNameType extends ByNameTypeImpl {
    def unapply(tp: Type): Option[Type] = tp match {
      case tp: Types.ExprType => Some(tp.underlying)
      case _ => None
    }
  }

  object MethodType extends MethodTypeImpl {
    def paramInfos(tp: MethodType): List[Type] = tp.paramInfos
    def instantiate(tp: MethodType)(params: List[Type]): Type = {
      tp.instantiate(params)
    }
    def unapply(tp: Type): Option[MethodType] = tp match {
      case tp: Types.MethodType => Some(tp)
      case _ => None
    }
  }

  /*------------------------------- symbols -------------------------------------*/
  object Symbol extends SymbolImpl {
    /** name of a member */
    def name(mem: Symbol): String = mem.showName

    /** type of a member with respect to a prefix */
    def asSeenFrom(mem: Symbol, prefix: Type): Type = mem.asSeenFrom(prefix).info

    def isCase(sym: Symbol): Boolean = sym.is(Flags.Case)
    def isTrait(sym: Symbol): Boolean = sym.is(Flags.Trait)
    def isPrivate(sym: Symbol): Boolean = sym.is(Flags.Private)
    def isProtected(sym: Symbol): Boolean = sym.is(Flags.Protected)
    def isOverride(sym: Symbol): Boolean = sym.is(Flags.Override)
    def isFinal(sym: Symbol): Boolean = sym.is(Flags.Final)
    def isImplicit(sym: Symbol): Boolean = sym.is(Flags.Implicit)
    def isLazy(sym: Symbol): Boolean = sym.is(Flags.Lazy)
    def isSealed(sym: Symbol): Boolean = sym.is(Flags.Sealed)
    def isAbstract(sym: Symbol): Boolean = sym.is(Flags.Abstract)
    def isMutable(sym: Symbol): Boolean = sym.is(Flags.Mutable)
  }

  def ensureOwner(tree: tpd.Tree, owner: Symbol): tpd.Tree = {
    val froms = getOwners(tree)
    froms.foldRight(tree) { (from, acc) =>
      if (from eq owner) acc
      else new t.TreeOps(acc).changeOwner(from, owner)
    }
  }

  def getOwners(tree: tpd.Tree): List[Symbol] = {
    val owners = Set.empty[Symbol]
    new t.TreeTraverser {
      def traverse(tree: t.Tree)(implicit ctx: Context): Unit = tree match {
        case tree: t.DefTree => owners += tree.symbol.owner
        case _               => traverseChildren(tree)
      }
    }.traverse(tree)

    owners.toList
  }

  /*------------------------------- Denotations -------------------------------------*/
  type Denotation = Denotations.Denotation

  object Denotation extends DenotationImpl {
    def name(denot: Denotation): String = denot.symbol.name.show

    def info(denot: Denotation): Type = denot.info.dealias

    def symbol(denot: Denotation): Symbol = denot.symbol
  }
}
