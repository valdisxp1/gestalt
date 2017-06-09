class LocationTest extends TestSuite {
  test("location") {
    import Locations._
    val line = currentLine()
    assert(line == 3) // starts from 0
  }
}
