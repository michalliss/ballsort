package ballsort.frontend.domain
import neotype.*

type Color = Color.Type
object Color extends Newtype[Int] {
  override inline def validate(input: Int): Boolean =
    input >= 0

  extension (x: Color) inline def toInt = Color.unwrap(x)
}

type Ball = Ball.Type
object Ball extends Newtype[Int] {
  override inline def validate(input: Int): Boolean =
    input >= 0

  def fromColor(c: Int) = Ball.unsafeMake(c)

  extension (x: Ball) inline def color = Ball.unwrap(x)
}
