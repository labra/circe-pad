package es.weso.shex
import es.weso.rdf.nodes._

abstract sealed trait ShapeExpr

case class ShapeOr(shapeExprs: List[ShapeExpr]) extends ShapeExpr 

case class ShapeAnd(shapeExprs: List[ShapeExpr]) extends ShapeExpr 

case class ShapeNot(shapeExpr: ShapeExpr) extends ShapeExpr

case class NodeConstraint(nodeKind: NodeKind) extends ShapeExpr

case class Shape(
    virtual:Option[Boolean], 
    closed: Option[Boolean], 
    extra: Option[List[IRI]],
    expression: Option[TripleExpr],
    inherit: Option[ShapeLabel],
    semActs: Option[List[SemAct]]
)

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
case class TripleConstraint(
    inverse: Option[Boolean],
    negated: Option[Boolean],
    predicate: IRI,
    valueExpr: ShapeExpr,
    min: Option[Int],
    max: Option[Max],
    semActs: Option[List[SemAct]]
    )
    
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




