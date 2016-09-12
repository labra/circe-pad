package es.weso.shex
import es.weso.rdf.nodes._

abstract sealed trait ShapeExpr

case class ShapeOr(shapeExprs: List[ShapeExpr]) extends ShapeExpr

case class ShapeAnd(shapeExprs: List[ShapeExpr]) extends ShapeExpr

case class ShapeNot(shapeExpr: ShapeExpr) extends ShapeExpr

case class NodeConstraint(
    nodeKind: Option[NodeKind],
    datatype: Option[IRI],
    xsFacets: List[XsFacet],
    values: Option[List[ValueSetValue]]
    ) extends ShapeExpr

case class Shape(
    virtual:Option[Boolean],
    closed: Option[Boolean],
    extra: Option[List[IRI]],
    expression: Option[TripleExpr],
    inherit: Option[ShapeLabel],
    semActs: Option[List[SemAct]]
) extends ShapeExpr

case class ShapeRef(reference: IRI) extends ShapeExpr

case class ShapeExternal() extends ShapeExpr

object NodeConstraint {

  def empty = NodeConstraint(
      nodeKind = None,
      datatype = None,
      xsFacets = List(),
      values = None
  )

  def nodeKind(nk: NodeKind): NodeConstraint =
    NodeConstraint.empty.copy(nodeKind = Some(nk))

}

sealed trait XsFacet
sealed trait StringFacet extends XsFacet
case class Length(v: Int) extends StringFacet
case class MinLength(v:Int) extends StringFacet
case class MaxLength(v:Int) extends StringFacet
case class Pattern(p: String) extends StringFacet

sealed trait NumericFacet extends XsFacet
case class MinInclusive(n: NumericLiteral) extends NumericFacet
case class MinExclusive(n: NumericLiteral) extends NumericFacet
case class MaxInclusive(n: NumericLiteral) extends NumericFacet
case class MaxExclusive(n: NumericLiteral) extends NumericFacet
case class TotalDigits(n: Int) extends NumericFacet
case class FractionDigits(n: Int) extends NumericFacet

sealed trait NumericLiteral
case class NumericInt(n: Int) extends NumericLiteral
case class NumericDouble(n: Double) extends NumericLiteral
case class NumericDecimal(n: BigDecimal) extends NumericLiteral

sealed trait ValueSetValue
case class IRIValue(i: IRI) extends ValueSetValue
case class StringValue(s: String) extends ValueSetValue
case class DatatypeString(s: String, iri: IRI) extends ValueSetValue
case class LangString(s: String, lang: String) extends ValueSetValue
case class Stem(stem: IRI) extends ValueSetValue // Obsolete??
case class StemRange(stem: StemValue, exclusions: Option[List[ValueSetValue]]) extends ValueSetValue

sealed trait StemValue
case class IRIStem(iri: IRI) extends StemValue
case object Wildcard extends StemValue


case class SemAct(name: IRI, code: Option[String])

abstract sealed trait TripleExpr
case class EachOf(
    expressions: List[TripleExpr],
    min: Option[Int],
    max: Option[Max],
    semActs: Option[List[SemAct]],
    annotations: Option[List[Annotation]]
) extends TripleExpr


case class SomeOf(
    expressions: List[TripleExpr],
    min: Option[Int],
    max: Option[Max],
    semActs: Option[List[SemAct]],
    annotations: Option[List[Annotation]]
) extends TripleExpr

case class Inclusion(include: ShapeLabel)
  extends TripleExpr

case class TripleConstraint(
    inverse: Option[Boolean],
    negated: Option[Boolean],
    predicate: IRI,
    valueExpr: ShapeExpr,
    min: Option[Int],
    max: Option[Max],
    semActs: Option[List[SemAct]]
    ) extends TripleExpr

case class Annotation(predicate: IRI, obj: IRI)

abstract sealed trait Max
case object Star extends Max
case class IntMax(v: Int) extends Max

sealed trait NodeKind
case object IRIKind extends NodeKind
case object BNodeKind extends NodeKind
case object NonLiteralKind extends NodeKind
case object LiteralKind extends NodeKind

abstract sealed trait ShapeLabel
case class IRILabel(iri: IRI) extends ShapeLabel
case class BNodeLabel(bnode: BNodeId) extends ShapeLabel


object ShEx {

}
