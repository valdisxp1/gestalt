object Main {
  val buzz = Some(Bees.Bee("buzz")).collect{
    case Bees.Bee(value) => value
  }
}

object Bees {
  case class Bee(value: String)
}
