package examples
import cats._
import cats.data._
import cats.implicits._

object ImplicitsRecursive {
  
case class A(xs: List[B])

case class B(xs: List[A])

implicit lazy val showA: Show[A] = new Show[A] {
  def show(v: A) =
    s"A[${v.xs.map(_.show).mkString(",")}]"
}

implicit lazy val showB: Show[B] = new Show[B] {
  def show(v: B) =
    s"B[${v.xs.map(_.show).mkString(",")}]"
} 

val a1 = A(List(B(List(A(List()))),B(List())))

}
