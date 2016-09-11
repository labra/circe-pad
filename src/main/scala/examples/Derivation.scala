package examples
import cats.data.Xor, io.circe._
import io.circe.generic.auto._
import io.circe.parser._

object Deriv {
  
trait Base {
  val a: String
}
case class Derived1(a1: String, x: String) extends Base {
  val a = a1
}
case class Derived2(a2: String, y: Int) extends Base {
  val a = a2
}

implicit val baseDecoder: Decoder[Base] = Decoder.instance { c => 
  for {
    a <- c.downField("a").as[String]
    x <- c.downField("x").as[Option[String]]
    y <- c.downField("y").as[Option[Int]]
    d <- Xor.fromOption(
      x.map(new Derived1(a, _)).orElse(y.map(new Derived2(a, _))),
      DecodingFailure("Failed to decode Base: ", c.history)
    )
  } yield d

}

val s1 = """
  { "a" : "pepe", "x": "Any" } 
"""

val t1 = decode[Base](s1)
}