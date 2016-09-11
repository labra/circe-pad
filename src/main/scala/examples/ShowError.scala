package examples
import io.circe.jawn._
import cats.implicits._

object ShowError {
  
  val xs = decode[List[Int]]("[1, 2, 3, 4, true, 5]").swap.toOption.get.show

}