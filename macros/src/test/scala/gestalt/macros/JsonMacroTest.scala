class JsonMacroTest extends TestSuite {
  test("simple case class"){
    case class Name(first: String, last: String)
    val format = JsonMacros.format[Name]()
//    val formatSafe = format
    val formatSafe =  JsonMacros.formatSafe[Name]()

    import JsonMacros._
    val expectedJohnSmith = JsObject(Seq(
      "first" -> JsString("John"),
      "last" -> JsString("Smith")))
    assert(format.toJson(Name("John","Smith")) == expectedJohnSmith)
    assert(formatSafe.toJson(Name("John","Smith")) == expectedJohnSmith)

    val tom = Name("Tom","Sawyer")
    assert(format.fromJson(format.toJson(tom)) == Some(tom))
    assert(format.fromJson(JsObject(Nil)) == None)

    assert(formatSafe.fromJson(format.toJson(tom)) == Some(tom))
    assert(formatSafe.fromJson(JsObject(Nil)) == None)
  }

  test("composition test"){
    case class Name(first: String, last: String)
    case class Registration(name: Name, email: String)

    implicit val nameFormat: JsonMacros.Format[Name] = JsonMacros.format[Name]()
    implicit val registrationFormat: JsonMacros.Format[Registration] = JsonMacros.format[Registration]()
    import JsonMacros._

    val registration = Registration(Name("Tom","Sawyer"),"tom.s@mark.twain.book")
    val expectedJson = JsObject(Seq(
      "name" -> JsObject(Seq(
        "first" -> JsString("Tom"),
        "last" -> JsString("Sawyer"))),
      "email" -> JsString("tom.s@mark.twain.book")
      )
    )
    assert(registrationFormat.toJson(registration) == expectedJson)
    assert(registrationFormat.fromJson(registrationFormat.toJson(registration)) == Some(registration))
  }

/*  test("composition test safe"){
    case class Name(first: String, last: String)
    case class Registration(name: Name, email: String)

    implicit val nameFormat: JsonMacros.Format[Name] = JsonMacros.formatSafe[Name]()
    implicit val registrationFormat: JsonMacros.Format[Registration] = JsonMacros.formatSafe[Registration]()
    import JsonMacros._

    val registration = Registration(Name("Tom","Sawyer"),"tom.s@mark.twain.book")
    val expectedJson = JsObject(Seq(
      "name" -> JsObject(Seq(
        "first" -> JsString("Tom"),
        "last" -> JsString("Sawyer"))),
      "email" -> JsString("tom.s@mark.twain.book")
    )
    )
    assert(registrationFormat.toJson(registration) == expectedJson)
    assert(registrationFormat.fromJson(registrationFormat.toJson(registration)) == Some(registration))
  }*/
}