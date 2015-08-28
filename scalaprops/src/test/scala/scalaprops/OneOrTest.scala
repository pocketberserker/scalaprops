package scalaprops

import scalaz._
import scalaz.std.anyVal._

object OneOrTest extends Scalaprops {

  properties("IList") = {
    type F[A] = OneOr[IList, A]

    Properties.list(
      scalazlaws.order.all[F[Int]],
      scalazlaws.cobind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  properties("Mel") = {
    type F[A] = OneOr[NonEmptyList, A]

    Properties.list(
      scalazlaws.order.all[F[Int]],
      scalazlaws.applicative.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.comonad.all[F]
    )
  }

}
