package scalaprops

import scalaz._
import scalaz.std.anyVal._

object MaybeTTest extends Scalaprops {

  val testLawsIList = {
    type F[A] = MaybeT[IList, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val testLawsMaybe = {
    type F[A] = MaybeT[Maybe, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[MaybeT]

  import FunctionEqual._

  val hoist1 = scalazlaws.hoist.law1[MaybeT, Maybe, Maybe, Maybe]
  val hoist2 = scalazlaws.hoist.law1[MaybeT, IList, IList, IList]
  val hoist3 = scalazlaws.hoist.law2[MaybeT, Maybe]
  val hoist4 = scalazlaws.hoist.law2[MaybeT, IList]
}
