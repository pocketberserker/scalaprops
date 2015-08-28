package scalaprops

import scalaz.std.either._
import scalaz.std.anyVal._

object EitherTest extends Scalaprops {

  properties("bitraverse") = scalazlaws.bitraverse.all[Either]
  properties("associative") = scalazlaws.associative.all[Either]
  properties("order") = scalazlaws.order.all[Int Either Int]

  properties("laws1") = {
    type T[A] = Either[Int, A]
    Properties.list(
      scalazlaws.monad.all[T],
      scalazlaws.traverse.all[T]
    )
  }

}
