package scalaprops

import scalaz.std.map._
import scalaz.std.anyVal._

object MapTest extends Scalaprops {

  properties("Laws1") = {
    type F[A] = Map[Int, A]
    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.isEmpty.all[F],
      scalazlaws.align.all[F],
      scalazlaws.order.all[Map[Int, Int]]
    )
  }

  properties("Laws2") = scalazlaws.monoid.all[Map[Int, Int]]
}
