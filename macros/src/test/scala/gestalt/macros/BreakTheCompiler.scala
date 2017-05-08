class Main {
  runCode("abcde"){
    case class Name(first: String, last: String)
    val buzz = Some(Bees.Bee("buzz")).collect{
      case Bees.Bee(value) => value
    }
  }
  def runCode(comment: String)(code: => Unit) = code
}

object Bees {
  case class Bee(value: String)
}
