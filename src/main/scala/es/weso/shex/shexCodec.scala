package es.weso.shex
import io.circe._
import cats.data._
import es.weso.rdf.nodes._
import cats._
import cats.implicits._

object shexCodec {

implicit lazy val encodeIRI = new Encoder[IRI] {
  final def apply(iri: IRI): Json = Json.fromString(iri.str) 
}

implicit lazy val decodeIRI: Decoder[IRI] =  
  Decoder[String].map(IRI(_))

implicit lazy val encodeSemAct = new Encoder[SemAct] {
  final def apply(a: SemAct): Json = {
    mkObjectTyped("SemAct",
        List(field("name", a.name),
             optField("code", a.code))
   )
  }
}

implicit lazy val decodeSemAct: Decoder[SemAct] = Decoder.instance { c =>
 for {
   _ <- fixedFieldValue(c, "type", "SemAct")
   name <- fieldDecode[IRI](c, "name")
   code <- optFieldDecode[String](c, "code")
 } yield SemAct(name,code) 
}
  

  
implicit lazy val encodeMax = new Encoder[Max] {
  final def apply(a: Max): Json = a match {
   case Star => Json.fromString("*")
   case IntMax(n) => Json.fromInt(n)
  }
}

 
implicit lazy val decodeMax: Decoder[Max] = 
  Decoder[Int].map(n => IntMax(n)).or(
  Decoder[String].map(s => Star))
 

implicit lazy val encodeTripleConstraint = new Encoder[TripleConstraint] {
  final def apply(a: TripleConstraint): Json = 
        mkObjectTyped("TripleConstraint",
        List(optField("inverse", a.inverse),
             optField("negated", a.negated),
             field("predicate", a.predicate),
             // field("valueExpr", a.valueExpr)
             optField("min", a.min),
             optField("max", a.max),
             optField("semActs", a.semActs)
             )
   )

implicit lazy val decodeTripleConstraint: Decoder[TripleConstraint] = Decoder.instance { c =>
 for {
   _ <- fixedFieldValue(c, "type", "TripleConstraint")
   inverse <- optFieldDecode[Boolean](c, "inverse")
   negated <- optFieldDecode[Boolean](c, "negated")
   predicate <- fieldDecode[IRI](c, "predicate")
//   valueExpr <- fieldDecode[ShapeExpr](c, "valueExpr")
   min <- optFieldDecode[Int](c, "min")
   max <- optFieldDecode[Max](c, "max")
   semActs <- optFieldDecode[List[SemAct]](c, "semActs")
 } yield TripleConstraint(inverse,negated,predicate,NodeConstraint(IRIKind),min,max,semActs) 
}
  
}
// Utils...
  
def encodeOptFieldAsMap[A](name: String, m: Option[A])(implicit encoder: Encoder[A]): Map[String,Json] = 
 m match {
      case None => Map()
      case Some(v) => Map(name -> encoder(v))
 }

def field[A: Encoder](name: String, v: A): Option[(String, Json)] = {
  val encoder = implicitly[Encoder[A]] 
  Some(name, encoder(v))
}

def optField[A: Encoder](name: String, m: Option[A]): Option[(String,Json)] = {
  m match {
    case None => None
    case Some(v) => field(name,v)
  }
}

def mkObjectTyped(typeName: String, fields: List[Option[(String,Json)]]): Json = {
  val map = Map("type" -> Json.fromString(typeName)) ++
            fields.filter(_.isDefined).sequence.getOrElse(List()).toMap
  Json.fromJsonObject(JsonObject.fromMap(map))
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