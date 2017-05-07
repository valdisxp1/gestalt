class JsonMacroTest extends TestSuite {
  test("simple case class"){
    case class Name(first: String, last: String)

    val someBee: Option[Bees.Bee] = Some(Bees.Worker("buzz"))
    val test = someBee.collect{
      case Bees.Worker(value) => value
    }
  }
}

object Bees{
  final case class Worker(value: String) extends Bee
  sealed trait Bee
}