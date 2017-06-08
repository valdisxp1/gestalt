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
    val tpe: Type = T.tpe
    if (!tpe.isCaseClass) {
      error("Not a case class", T.pos)
      q"???"
    } else {
      val fields = tpe.caseFields
      val fieldsWithTypes = fields.map {
        f => f-> f.info
      }

      case class JsonItem(name: String, value: Ident, pairOut: TermTree, readOption: ValDef, implicitFormat: Option[ValDef])
      val jsonItems: List[JsonItem] = fieldsWithTypes.map {
        case (field, stringType) if stringType =:= Type.typeRef("java.lang.String") =>
          val name = field.name
          JsonItem(name,
            pairOut = Tuple(List(Lit(name), q"JsString(o.$name)")),
            value = Ident(name),
            readOption = q"val $name = obj.firstValue(${Lit(name)}).collect{case JsString(value) => value}",
            implicitFormat = None
          )
        case (field, otherType) =>
          val name = field.name
          val implFormaterName = name + "_formatter"
          val formatterIdent = Ident(implFormaterName)
          JsonItem(name,
            pairOut = Tuple(List(Lit(name), q"$formatterIdent.toJson(o.$name)")),
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
      q"""
          import JsonMacros._
          new Format[$T]{..${
        jsonItems.flatMap(_.implicitFormat).toList :+
          q"def toJson(o: $T) = JsObject(Seq(..${jsonItems.map(_.pairOut)}))" :+
          q"def fromJson(json: JsValue) = $fromJson"
      }}
        """
    }
  }


  /**
    * Safe variant - respects macro hygine to a greater degree than the other implementation.
    * Creates a {{{Format}}} for the case class.
    * Limitations:
    * 1. does not handle cyclic references, i.e. {{{case class A(field: A, ...)}}}
    * 2. does not handle optional arguments
    * @tparam T type of the case class
    * @return new anonymous Format class
    */
  def formatSafe[T](): Format[T] = meta {
    import scala.gestalt.options.unsafe

    val jsonStr = Type.termRef("<empty>.JsonMacros.JsString").toTree

    val formatType     = Type.typeRef("<empty>.JsonMacros.Format")
    val jsValueType     = Type.typeRef("<empty>.JsonMacros.JsValue")
    val formatTree     = formatType.toTree
    val implicitlyType = Type.termRef("scala.Predef.implicitly")
    val implicitlyTree = implicitlyType.toTree
    def getImplicit(tp: Type): tpd.Tree = implicitlyTree.appliedToTypes(formatTree.appliedToTypes(tp.toTree))

    val tpe: Type = T.tpe
    if (!tpe.isCaseClass) {
      error("Not a case class", T.pos)
      q"???"
    } else {
      val fields = tpe.caseFields
      val fieldsWithTypes = fields.map {
        f => f -> f.info
      }

      case class JsonItem(name: String, value: Ident, pairOut: tpd.Tree => tpd.Tree, readOption: ValDef, implicitFormat: Option[ValDef])
      val jsonItems: List[JsonItem] = fieldsWithTypes.map {
        case (field, stringType) if stringType =:= Type.typeRef("java.lang.String") =>
          val name = field.name
          JsonItem(name,
            pairOut = (o: tpd.Tree) => Tuple(Lit(field.name).typed :: jsonStr.appliedTo(o.select(field.name)) :: Nil),
            value = Ident(name),
            readOption = q"val $name = obj.firstValue(${Lit(name)}).collect{case JsString(value) => value}",
            implicitFormat = None
          )
        case (field, otherType) =>
          val name = field.name
          val implFormaterName = name + "_formatter"
          val formatterIdent = Ident(implFormaterName)
          JsonItem(name,
            pairOut = (o: tpd.Tree) => Tuple(Lit(field.name).typed :: getImplicit(otherType).select("toJson").appliedTo(o.select(field.name)) :: Nil),
            value = Ident(name),
            readOption = q"val $name = obj.firstValue(${Lit(name)}).flatMap(x =>$formatterIdent.fromJson(x))",
            implicitFormat = Some(q"val $implFormaterName=implicitly[Format[${otherType.toTree}]]")
          )
      }
      val allDefined = q"${jsonItems.map(i => q"${i.value}.isDefined").reduceLeft((a, b) => q"$a && $b")}"
      val construction = NewInstance(T, List(jsonItems.map(i => q"${i.value}.get")))
      val fromJson =
        q"""def fromJson(json: JsValue) =  json match{
              case obj: JsObject =>
               {..${
          jsonItems.map(_.readOption) :+
            q"if($allDefined) Some($construction) else None"
        }}
              case other => None
            }"""
      val toJson = DefDef("toJson", ("o", tpe) :: Nil, jsValueType) {
        case List(o) =>
          val values = jsonItems.map(_.pairOut(o))

          val jsonObject = Ident(Type.termRef("JsonMacros.JsObject.apply").symbol)
          val scalaSeq = Ident(Type.termRef("scala.collection.immutable.Seq.apply").symbol)
          val itemType = Type.typeRef("scala.Tuple2").appliedTo(
            Type.typeRef("scala.String"),
            jsValueType
          )
          val seqLiteral = SeqLiteral(values, itemType)
          jsonObject.appliedTo(scalaSeq.appliedToTypes(itemType.toTree).appliedTo(seqLiteral))
      }.wrap
      q"""
          import JsonMacros._
          new Format[$T]{..${
        jsonItems.flatMap(_.implicitFormat).toList :+
          toJson :+ fromJson
      }}
        """
    }
  }
}
