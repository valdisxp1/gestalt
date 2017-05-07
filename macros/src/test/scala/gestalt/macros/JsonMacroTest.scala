class JsonMacroTest extends TestSuite {
  test("simple case class"){
    case class Name(first: String, last: String)
    val format = new JsonMacros.Format[Name] {
      def toJson(o: Name) =
        JsonMacros.JsObject(
          Seq("first".->(JsonMacros.JsString(o.first)),
            "last".->(JsonMacros.JsString(o.last))
          )
        )

      def fromJson(json: JsonMacros.JsValue) =
        json match {
          case obj: JsonMacros.JsObject =>
            val first =
              obj.firstValue("first").collect {
                case JsonMacros.JsString(value) => value
              }
            val last = obj.firstValue("last").collect {
              case JsonMacros.JsString(value) => value
            }
            if (first.isDefined.&&(last.isDefined)) Some(null) else None
          case other => None
        }
    }
    import JsonMacros._
    assert(format.toJson(Name("John","Smith")) == JsObject(Seq(
      "first" -> JsString("John"),
      "last" -> JsString("Smith"))))
  }
}