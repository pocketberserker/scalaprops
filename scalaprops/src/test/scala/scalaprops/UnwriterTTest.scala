package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object UnwriterTTest extends Scalaprops {

  properties("id") = {
    type F[A] = Unwriter[Int, A]

    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  properties("Maybe1") = {
    type F[A] = UnwriterT[Maybe, Int, A]

    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  properties("Maybe2") = {
    type F[A, B] = UnwriterT[Maybe, A, B]

    scalazlaws.bitraverse.all[F]
  }

}
