import scala.gestalt._

object JsonMacros {

  sealed trait JsValue
  final case class JsString(value: String)
  final case class JsArray(items: Seq[(String, JsValue)])

  trait Format[A] {
    def toJson(o: A): JsValue
    def fromJson(json: JsValue): Option[A]
  }

  def format[T]() = meta {
    q"null"
  }
}
