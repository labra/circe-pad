package examples
import io.circe._, io.circe.generic.semiauto._, io.circe.parser._

case class Coord(x: Double, y: Double)

object Coord {
  implicit val decodeCoord: Decoder[Coord] =
    Decoder[(Double, Double)].map(p => Coord(p._1, p._2)).or(
      Decoder[(Double, Double, Double)].map(p => Coord(p._1, p._2))
    )
}

object Test {
  
val x = Coord(23,34)
  
val rawJson: String = """
[2.3, 4.5 ]
"""
}

