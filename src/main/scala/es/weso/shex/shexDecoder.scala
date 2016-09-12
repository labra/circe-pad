package es.weso.shex
import io.circe._
import cats.data._
import es.weso.rdf.nodes._
import cats._
import cats.implicits._

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
      case "NodeConstraint" => c.as[NodeConstraint]
      case "Shape" => c.as[Shape]
      case "ShapeRef" => c.as[ShapeRef]
      case "ShapeExternal" => c.as[ShapeExternal]
    }
}

implicit lazy val decodeShapeOr: Decoder[ShapeOr] = Decoder.instance { c =>
  ???
}


implicit lazy val decodeShapeAnd: Decoder[ShapeAnd] = Decoder.instance { c =>
  ???
}


implicit lazy val decodeShapeNot: Decoder[ShapeNot] = Decoder.instance { c =>
  ???
}


implicit lazy val decodeNodeConstraint: Decoder[NodeConstraint] = Decoder.instance { c =>
  for {
    _ <- fixedFieldValue(c, "type", "NodeConstraint")
    nodeKind <- optFieldDecode[NodeKind](c, "nodeKind")
  } yield NodeConstraint(nodeKind, None, List(), None)
}

implicit lazy val decodeNodeKind: Decoder[NodeKind] = Decoder.instance { c =>
  c.downField("nodeKind").as[String].flatMap {
    case "iri" => IRIKind.right
    case "bnode" => BNodeKind.right
    case "nonliteral" => NonLiteralKind.right
    case "literal" => LiteralKind.right
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
