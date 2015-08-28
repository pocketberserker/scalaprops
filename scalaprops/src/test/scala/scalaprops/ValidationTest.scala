package scalaprops

import scalaz._
import scalaz.std.anyVal._

object ValidationTest extends Scalaprops {

  properties("Laws1") = {
    type F[A] = ValidationNel[Int, A]

    Properties.list(
      scalazlaws.applicative.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.plus.all[F]
    )
  }

  properties("Laws2") = scalazlaws.associative.all[Validation]
  properties("Laws3") = scalazlaws.bitraverse.all[Validation]
  properties("Laws4") = scalazlaws.monoid.all[Validation[Int, Int]]
  properties("Order") = scalazlaws.order.all[Validation[Int, Int]]
}
