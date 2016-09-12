package es.weso.shex
import io.circe._
import io.circe.syntax._
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf.nodes._

object shexDecoder {

implicit lazy val decodeIRI: Decoder[IRI] =
  Decoder[String].map(IRI(_))

implicit lazy val decodeSemAct: Decoder[SemAct] = Decoder.instance { c =>
 for {
   _ <- fixedFieldValue(c, "type", "SemAct")
   name <- fieldDecode[IRI](c, "name")
   code <- optFieldDecode[String](c, "code")
 } yield SemAct(name,code)
}

implicit lazy val decodeMax: Decoder[Max] =
  Decoder[Int].map(n => IntMax(n)).or(
  Decoder[String].map(s => Star))

implicit lazy val decodeShapeExpr: Decoder[ShapeExpr] = Decoder.instance { c =>
  c.downField("type").as[String].flatMap {
      case "ShapeOr" => c.as[ShapeOr]
      case "ShapeAnd" => c.as[ShapeAnd]
      case "ShapeNot" => c.as[ShapeNot]
      case "NodeConstraint" =>  c.as[NodeConstraint]
      case "Shape" => c.as[Shape]
      case "ShapeRef" => c.as[ShapeRef]
      case "ShapeExternal" => c.as[ShapeExternal]
    }
}

implicit lazy val decodeShapeOr: Decoder[ShapeOr] = Decoder.instance { c =>
 for {
   _ <- fixedFieldValue(c, "type", "ShapeOr")
   ses <- fieldDecode[List[ShapeExpr]](c, "shapeExprs")
 } yield ShapeOr(ses)
}


implicit lazy val decodeShapeAnd: Decoder[ShapeAnd] = Decoder.instance { c =>
 for {
   _ <- fixedFieldValue(c, "type", "ShapeAnd")
   ses <- fieldDecode[List[ShapeExpr]](c, "shapeExprs")
 } yield ShapeAnd(ses)
}


implicit lazy val decodeShapeNot: Decoder[ShapeNot] = Decoder.instance { c =>
 for {
   _ <- fixedFieldValue(c, "type", "ShapeNot")
   se <- fieldDecode[ShapeExpr](c, "shapeExpr")
 } yield ShapeNot(se)
}


implicit lazy val decodeNodeConstraint: Decoder[NodeConstraint] = Decoder.instance { c =>
  for {
    _ <- fixedFieldValue(c, "type", "NodeConstraint")
    nodeKind <- optFieldDecode[NodeKind](c, "nodeKind")
    datatype <- optFieldDecode[IRI](c, "datatype")
    values <- optFieldDecode[List[ValueSetValue]](c,"values")
  } yield NodeConstraint(nodeKind, datatype, getXsFacets(c), values)
}

def getXsFacets(c: HCursor): List[XsFacet] = {
  val fields = c.fields.getOrElse(List())
  val rs = fields.map(extractXsFacet(_,c))
  ???
}

def extractXsFacet(name: String, c: HCursor): Xor[DecodingFailure,Option[XsFacet]] = {
  name match {
    case "length" => c.field(name).as[Int].map(n => Some(Length(n)))
    case "minlength" => c.field(name).as[Int].map(n => Some(MinLength(n)))
    case "maxlength" => c.field(name).as[Int].map(n => Some(MaxLength(n)))
    case _ => None.right
  }
}

implicit lazy val decodeNodeKind: Decoder[NodeKind] = Decoder.instance { c =>
  c.field("nodeKind").as[String].flatMap {
    case "iri" => IRIKind.right
    case "bnode" => BNodeKind.right
    case "nonLiteral" => NonLiteralKind.right
    case "literal" => LiteralKind.right
    case other => 
      Xor.left(DecodingFailure(s"Decoding nodeKind. Unexpected value $other", Nil))
  }
}

implicit lazy val decodeShape: Decoder[Shape] = Decoder.instance { c =>
  ???
}


implicit lazy val decodeShapeRef: Decoder[ShapeRef] = Decoder.instance { c =>
 for {
   _ <- fixedFieldValue(c, "type", "ShapeRef")
   reference <- fieldDecode[IRI](c, "reference")
 } yield ShapeRef(reference)
}


implicit lazy val decodeShapeExternal: Decoder[ShapeExternal] = Decoder.instance { c =>
 for {
   _ <- fixedFieldValue(c, "type", "ShapeExternal")
 } yield ShapeExternal()
}

implicit lazy val decodeValueSetValue: Decoder[ValueSetValue] = Decoder.instance { c =>
  c.as[String].flatMap(s => IRIValue(IRI(s)).right)
}

implicit lazy val decodeTripleConstraint: Decoder[TripleConstraint] = Decoder.instance { c =>
 for {
   _ <- fixedFieldValue(c, "type", "TripleConstraint")
   inverse <- optFieldDecode[Boolean](c, "inverse")
   negated <- optFieldDecode[Boolean](c, "negated")
   predicate <- fieldDecode[IRI](c, "predicate")
   valueExpr <- fieldDecode[ShapeExpr](c, "valueExpr")
   min <- optFieldDecode[Int](c, "min")
   max <- optFieldDecode[Max](c, "max")
   semActs <- optFieldDecode[List[SemAct]](c, "semActs")
 } yield TripleConstraint(inverse,negated,predicate,NodeConstraint.empty,min,max,semActs)
}

// Utils...

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
