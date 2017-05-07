class JsonMacroTest extends TestSuite {
  test("simple case class"){
    case class Name(first: String, last: String)
    import JsonMacros._

    val obj: JsObject = JsObject(Nil)
    val first =
      obj.firstValue("first").collect {
        case JsonMacros.JsString(value) => value
      }
  }
}