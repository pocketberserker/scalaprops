package scalaprops

import scalaz._
import scalaz.std.anyVal._

object OneAndTest extends Scalaprops {

  val testIList = {
    type F[A] = OneAnd[IList, A]

    Properties.list(
      scalazlaws.order.all[F[Int]],
      scalazlaws.monad.all[F],
      scalazlaws.zip.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.align.all[F]
    )
  }

  val testMaybe = {
    type F[A] = OneAnd[Maybe, A]

    Properties.list(
      scalazlaws.order.all[F[Int]],
      scalazlaws.monad.all[F],
      scalazlaws.zip.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.align.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val testNel = {
    type F[A] = OneAnd[NonEmptyList, A]

    Properties.list(
      scalazlaws.order.all[F[Int]],
      scalazlaws.bind.all[F],
      scalazlaws.zip.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.align.all[F],
      scalazlaws.plus.all[F]
    )
  }

}
