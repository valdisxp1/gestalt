class Main {
  val buzz = Some(Bees.Bee("buzz")).collect {
    case Bees.Bee(value) => value
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    new Main
  }
}

object Bees {
  case class Bee(value: String)
}
