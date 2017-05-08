class JsonMacroTest extends TestSuite {
  test("simple case class"){
    case class Name(first: String, last: String)
    val buzz = Some(Bees.Bee("buzz")).collect{
      case Bees.Bee(value) => value
    }

  }
}