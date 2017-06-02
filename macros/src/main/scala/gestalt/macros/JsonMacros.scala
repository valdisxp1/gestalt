import scala.gestalt.api._

object JsonMacros {

  sealed trait JsValue
  final case class JsString(value: String) extends JsValue

  final case class JsObject(items: Seq[(String, JsValue)]) extends JsValue {
    def firstValue(key: String) = items.find(_._1 == key).map(_._2)
  }

  trait Format[A] {
    def toJson(o: A): JsValue
    def fromJson(json: JsValue): Option[A]
  }

  /**
    * Creates a {{{Format}}} for the case class.
    * Limitations:
    * 1. does not handle cyclic references, i.e. {{{case class A(field: A, ...)}}}
    * 2. does not handle optional arguments
    * @tparam T type of the case class
    * @return new anonymous Format class
    */
  def format[T](): Format[T] = meta {
    import scala.gestalt.options.unsafe

    val implicitlyType = Type.termRef("implicitly")
    val implicitlyTree = implicitlyType.toTree
    val formatType     = Type.typeRef("JsonMacros.Format")
    val formatTree     = formatType.toTree
    val jsonStr = Ident(Type.termRef("JsonMacros.JsString").symbol.get)

    def getImplicit(tp: Type): tpd.Tree = implicitlyTree.appliedToTypes(formatTree.appliedToTypes(tp.toTree))

    val tpe: Type = T.tpe
    if (!tpe.isCaseClass) {
      error("Not a case class", T.pos)
      q"???"
    } else {
      val fieldsWithTypes = tpe.caseFields.map {
        f => f -> f.info
      }

      case class JsonItem(name: String, value: Ident, pairOut: tpd.Tree => tpd.Tree, readOption: ValDef, implicitFormat: Option[ValDef])
      val jsonItems: List[JsonItem] = fieldsWithTypes.map {
        case (field, stringType) if stringType =:= Type.typeRef("java.lang.String") =>
          val name = field.name
          JsonItem(name,
            pairOut = o => Tuple(Lit(field.name)(cap) :: jsonStr.appliedTo(o.select(field.name)) :: Nil),
            value = Ident(name),
            readOption = q"val $name = obj.firstValue(${Lit(name)}).collect{case JsString(value) => value}",
            implicitFormat = None
          )
        case (field, otherType) =>
          val name = field.name
          val implFormaterName = name + "_formatter"
          val formatterIdent = Ident(implFormaterName)
          JsonItem(name,
            pairOut = o => Tuple(Lit(field.name)(cap) :: getImplicit(tp).select("toJson").apliedTo(o.select(field.name))),
            value = Ident(name),
            readOption = q"val $name = obj.firstValue(${Lit(name)}).flatMap(x =>$formatterIdent.fromJson(x))",
            implicitFormat = Some(q"val $implFormaterName=implicitly[Format[${otherType.toTree}]]")
          )
      }
      val allDefined = q"${jsonItems.map(i => q"${i.value}.isDefined").reduceLeft((a, b) => q"$a && $b")}"
      val construction = NewInstance(T, List(jsonItems.map(i => q"${i.value}.get")))
      val fromJson =
        q"""json match{
              case obj: JsObject =>
               {..${
          jsonItems.map(_.readOption) :+
            q"if($allDefined) Some($construction) else None"
        }}
              case other => None
            }"""

      val toJson = DefDef("toJson", ("o", T.tpe) :: Nil) {
        case List(o) =>
          val values = jsonItems.map(_.pairOut(o))

          val jsonObject = Ident(Type.termRef("JsonMacros.JsObject.apply").symbol.get)
          val scalaSeq = Ident(Type.termRef("scala.collection.immutable.Seq.apply").symbol.get)
          val itemType = Type.typeRef("scala.Tuple2").appliedTo(
            Type.typeRef("scala.String"),
            Type.typeRef("JsonMacros.JsValue")
          )
          val seqLiteral = SeqLiteral(values, itemType)
          jsonObject.appliedTo(scalaSeq.appliedToTypes(itemType.toTree).appliedTo(seqLiteral))
      }

/*      val fromJson = DefDef("fromJson", ("json", Type.typeRef("JsValue")) :: Nil) {
        case List(json) =>
          ???
      }*/

      val stats = jsonItems.flatMap(_.implicitFormat) +: List(toJson, fromJson)

      q"""
          import JsonMacros._
          new Format[$T]{ ..$stats }
        """
    }
  }
}
