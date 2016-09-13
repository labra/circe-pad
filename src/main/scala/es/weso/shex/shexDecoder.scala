package es.weso.shex
import io.circe._
import io.circe.syntax._
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf.nodes._
import util.matching._

object shexDecoder {

implicit lazy val decodeSchema: Decoder[Schema] = Decoder.instance { c =>
  for {
   _ <- fixedFieldValue(c, "type", "Schema")
   prefixes <- optFieldDecodeMap[Prefix,IRI](c, "prefixes")
   base <- optFieldDecode[IRI](c, "base")
   startActs <- optFieldDecode[List[SemAct]](c, "startActs")
   start <- optFieldDecode[ShapeExpr](c, "start")
   shapes <- optFieldDecodeMap[ShapeLabel,ShapeExpr](c, "shapes")
 } yield Schema(prefixes,base,startActs,start,shapes)
}

implicit lazy val decodePrefix: Decoder[Prefix] = 
  Decoder[String].map(Prefix(_))
  
implicit lazy val keyDecoderPrefix: KeyDecoder[Prefix] = 
  KeyDecoder.instance { str => parsePrefix(str).toOption }
  

implicit lazy val decodeShapeLabel: Decoder[ShapeLabel] = 
  Decoder[String].emap(str => parseShapeLabel(str))
  
implicit lazy val keyDecoderShapeLabel: KeyDecoder[ShapeLabel] =
  KeyDecoder.instance { str => parseShapeLabel(str).toOption }

  
  
implicit lazy val decodeBNodeId: Decoder[BNodeId] = 
  Decoder[String].map(BNodeId(_))
  
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
    xsFacets <- getXsFacets(c)
  } yield NodeConstraint(nodeKind, datatype, xsFacets, values)
}

def getXsFacets(c: HCursor): Decoder.Result[List[XsFacet]] = {
  val fields = c.fields.getOrElse(List())
  fields.map(extractXsFacet(_,c)).sequence match {
    case Xor.Left(e) => Xor.Left(e)
    case Xor.Right(ls) => Xor.Right(ls.filter(_.isDefined).sequence.getOrElse(List()))
  }
}

def extractXsFacet(name: String, c: HCursor): Xor[DecodingFailure,Option[XsFacet]] = {
  name match {
    case "length" => c.get[Int](name).map(n => Some(Length(n))) 
    case "minlength" => c.get[Int](name).map(n => Some(MinLength(n)))
    case "maxlength" => c.get[Int](name).map(n => Some(MaxLength(n)))
    case "pattern" => c.get[String](name).map(p => Some(Pattern(p)))
    case "mininclusive" => c.get[NumericLiteral](name).map(p => Some(MinInclusive(p)))
    case "minExclusive" => c.get[NumericLiteral](name).map(p => Some(MinInclusive(p)))
    case "maxinclusive" => c.get[NumericLiteral](name).map(p => Some(MaxInclusive(p)))
    case "maxexclusive" => c.get[NumericLiteral](name).map(p => Some(MaxExclusive(p)))
    case _ => None.right
  }
}

implicit lazy val decodeNumericLiteral: Decoder[NumericLiteral] = 
  Decoder[Int].map(n => NumericInt(n)).or(
  Decoder[Double].map(n => NumericDouble(n)).or(
  Decoder[BigDecimal].map(n => NumericDecimal(n))
  ))

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

def optFieldDecodeMap[A: KeyDecoder, B: Decoder](c: HCursor, name: String): Decoder.Result[Option[Map[A,B]]] = {
    val x = c.downField(name)
    if (x.succeeded) x.as[Map[A,B]].map(Some(_))
    else Xor.right(None)
  }

def parsePrefix(str: String): Xor[String,Prefix] = 
  str match {
  case prefixRegex(p) => Prefix(p).right
  case _ => Xor.Left(s"$str doesn't match prefix regex $prefixRegex") 
}

def parseShapeLabel(str: String): Xor[String,ShapeLabel] = {
  str match {
    // Be careful with the order...
    case bNodeRegex(bNodeId) => BNodeLabel(BNodeId(bNodeId)).right
    case iriRegex(i) => IRILabel(IRI(i)).right
    case _ => Xor.left(s"$str doesn't match IRI or BNode")
  }
}

def parseLang(str: String): Xor[String,LangString] = 
  str match {
  case langRegex(s,l) => LangString(s,l).right
  case _ => Xor.Left(s"$str doesn't match IRI regex $iriRegex")
}

def parseIRI(str: String): Xor[String,IRI] = 
  str match {
  case iriRegex(i) => IRI(str).right
  case _ => Xor.Left(s"$str doesn't match IRI regex $iriRegex")
}
  
lazy val prefixRegex: Regex = "^(.*)$".r // PN_PREFIX_STR.r
lazy val iriRegex: Regex = "^(.*)$".r
lazy val bNodeRegex: Regex = "^_:(.*)$".r
lazy val stringRegex: Regex = "^\"(.*)\"$".r
lazy val langRegex: Regex = "^\"(.*)\"@(.*)$".r
lazy val datatypeRegex: Regex = "^\"(.*)\"^^(.*)$".r


/*
def startMiddleAltRep_Str(start: String, middleAlt: String, repEnd: String): String = {
    "(" + start + "(((" + repEnd + "|" + middleAlt + ")*(" + repEnd + "))?)" + ")"
}

lazy val PN_PREFIX_STR = startMiddleAltRep_Str(PN_CHARS_BASE, "\\.", PN_CHARS_STR)
lazy val PN_CHARS_STR = PN_CHARS_U + """|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040]"""
lazy val PN_CHARS_U = PN_CHARS_BASE + "|_"
lazy val PN_CHARS_BASE =
    """[a-zA-Z\u00C0-\u00D6\u00D8-\u00F6""" +
      """\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF""" +
      """\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF""" +
      """\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD""" +
      """\x{10000}-\x{EFFFF}]"""
*/
}

