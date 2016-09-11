package examples

import cats.implicits._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
 
sealed trait Metric
case object Equal extends Metric
case object Ignore extends Metric
final case class Within(center: Double, distance: Double) extends Metric
final case class QuestionDescription(question: String, metric: Metric)
 
object QuestionDescription {
  
  final case class Switcher(typ: String, payload: Json)
 
  implicit val metricDecoder = Decoder[Switcher].flatMap(metricFromSwitcher)
 
  def metricFromSwitcher(s: Switcher): Decoder[Metric] = s.typ match {
    case "ignore" => Ignore.pure[Decoder].widen[Metric]
    case "equal"  => Equal.pure[Decoder].widen[Metric]
    case "within" => Decoder[Within].prepare(_ => s.payload.hcursor.acursor).widen[Metric]
    case _        => Decoder.failedWithMessage(s"Unknown type ${s.typ}")
  }
 
  def parse(s: String) = decode[QuestionDescription](s)
}
 
object Main {
  def main(args: Array[String]): Unit =
    println(QuestionDescription.parse("""{"question": "qu?", "metric": {"typ": "within", "payload": { "center": 0, "distance": 1}}}"""))
}

