package es.weso.shex
import cats._
import cats.implicits._
import es.weso.rdf.nodes._

object shexShow {
  
implicit lazy val showMax = new Show[Max] {
  final def show(a: Max): String = a match {
   case Star => "*"
   case IntMax(n) => n.show
  }
}

implicit lazy val showIRI = new Show[IRI] {
  final def show(iri: IRI): String = iri.str 
}

implicit lazy val showSemAct = new Show[SemAct] {
  final def show(a: SemAct): String =
    "SemAct(" + a.name.show + "," + optShow(a.code) + ")"
 }

    
def optShow[A: Show](m: Option[A]): String = 
  m match {
  case None => "-"
  case Some(v) => v.show
}

}