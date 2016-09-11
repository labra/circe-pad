package examples
import io.circe._
import cats.data._

abstract sealed trait Max 
case object Star extends Max
case class IntMax(v: Int) extends Max

case class TripleConstraint(
    inverse: Option[Boolean],
    negated: Option[Boolean],
    predicate: String,
    min: Option[Int],
    max: Option[Max]
)
    
object implicits {
  
  
  def encodeOptFieldAsMap[A](name: String, m: Option[A])(implicit encoder: Encoder[A]): Map[String,Json] = 
    m match {
      case None => Map()
      case Some(v) => Map(name -> encoder(v))
    }
  
  implicit lazy val encodeMax = new Encoder[Max] {
    final def apply(a: Max): Json = a match {
      case Star => Json.fromString("*")
      case IntMax(n) => Json.fromInt(n)
    }
  }
  
  implicit lazy val decodeMax: Decoder[Max] = 
    Decoder[Int].map(n => IntMax(n)).or(
    Decoder[String].map(s => Star)
    )
    
  implicit lazy val encodeTripleConstraint = new Encoder[TripleConstraint] {
    final def apply(tc: TripleConstraint): Json = 
      Json.fromJsonObject(
         JsonObject.fromMap(
          encodeOptFieldAsMap("inverse", tc.inverse) ++ 
          encodeOptFieldAsMap("negated", tc.negated) ++
          encodeOptFieldAsMap("min", tc.min) ++
          encodeOptFieldAsMap("max", tc.max) ++
          Map("predicate" -> Json.fromString(tc.predicate)) ++
          Map("type" -> Json.fromString("TripleConstraint"))
         )
      ) 
  }
  
  implicit val decodeTripleConstraint: Decoder[TripleConstraint] = Decoder.instance { c =>
      for {
        _ <- fixedFieldValue(c, "type", "TripleConstraint")
        inverse <- optFieldDecode[Boolean](c, "inverse")
        negated <- optFieldDecode[Boolean](c, "negated")
        min <- optFieldDecode[Int](c, "min")
        max <- optFieldDecode[Max](c, "max")
        predicate <- fieldDecode[String](c, "predicate")
      } yield TripleConstraint(inverse,negated,predicate,min,max) 
  }
  
  def fixedFieldValue(c: HCursor, name: String, value: String): Decoder.Result[String] =
    c.downField(name).as[String].flatMap(v => 
      if (v == value) 
        Xor.right(v) 
      else 
        Xor.left(DecodingFailure(s"Required $value for field $name but got $v", Nil))
  )

  def fieldDecode[A: Decoder](c: HCursor,name: String): Decoder.Result[A] =
    c.downField(name).as[A]
  
  def optFieldDecode[A: Decoder](c: HCursor, name: String): Decoder.Result[Option[A]] = {
    val x = c.downField(name)
    if (x.succeeded) x.as[A].map(Some(_))
    else Xor.right(None)
  }
  
}

object E1 {
  val x = TripleConstraint(Some(true),None,"x", Some(3),Some(Star))
  
  val str = """
    { "type": "TripleConstraint",
      "negated": true,
      "inverse": false,
      "predicate" : "x",
      "min": 3,
      "max": "*"
    }
    """
}