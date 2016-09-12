package es.weso.shex
import org.scalatest._
import shexEncoder._
import shexDecoder._
import shexShow._
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf.nodes._

class shexCodecTest extends FunSpec with Matchers with EitherValues {
  describe("Max codec") {
   codecValueTest[Max](IntMax(5))
   codecValueTest[Max](Star)
   codecStrTest[Max](""""*"""","\"*\"")
   codecStrTest[Max](""""5"""", "5")
   codecStrTest[Max]("""5""", "5")
  }

  describe("SemAct codec") {
   codecValueTest[SemAct](SemAct(IRI("x"),None))
   codecValueTest[SemAct](SemAct(IRI("x"),Some("blah")))
  }

  describe("ShapeExpr codec") {
   codecValueTest[ShapeExpr](NodeConstraint(Some(IRIKind), None, List(), None))
   codecValueTest[ShapeExpr](NodeConstraint(Some(LiteralKind), None, List(), None))
   codecValueTest[ShapeExpr](NodeConstraint(Some(NonLiteralKind), None, List(), None))
   codecValueTest[ShapeExpr](NodeConstraint(Some(BNodeKind), None, List(), None))
   codecValueTest[ShapeExpr](NodeConstraint(None, Some(IRI("http://datatype.org/int")), List(), None))
   codecValueTest[ShapeExpr](NodeConstraint(None, Some(IRI("http://datatype.org/int")), List(Length(0)), None))
   codecValueTest[ShapeExpr](ShapeRef(IRI("x")))
   codecValueTest[ShapeExpr](ShapeExternal())
  }


  def codecValueTest[A: Encoder: Decoder: Show](v: A): Unit = {
    it(s"Should encode and decode ${v.show}") {
     val str = v.asJson.spaces4
     val result = decode[A](str)
     if (result == v.right)
       info(s"Encoded as $str")
     else
       fail(s"Encoded value $v as $str was not equal to ${v.right}. Result: ${result}")
    }
  }

  def codecStrTest[A: Encoder: Decoder: Manifest](str: String, expected: String): Unit = {
   it(s"Should decode $str and obtain $expected through decoder of type ${manifest[A].runtimeClass.getSimpleName}") {
    decode[A](str).fold(e => fail(s"Error parsing $str: $e"),
             _.asJson.noSpaces should be(expected))
    }
  }


}
