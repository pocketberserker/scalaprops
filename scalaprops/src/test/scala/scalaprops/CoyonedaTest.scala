package scalaprops

import scalaz._
import scalaz.std.anyVal._

object CoyonedaTest extends Scalaprops {

  properties("OrderMaybe") = scalazlaws.order.all[Coyoneda[Maybe, Int]]
  properties("OrderIList") = scalazlaws.order.all[Coyoneda[IList, Int]]

  properties("Nel") = {
    type F[A] = Coyoneda[NonEmptyList, A]

    Properties.list(
      scalazlaws.traverse1.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }

  properties("Maybe") = {
    type F[A] = Coyoneda[Maybe, A]

    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.cobind.all[F]
    )
  }

  properties("IList") = {
    type F[A] = Coyoneda[IList, A]

    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.cobind.all[F]
    )
  }
}
