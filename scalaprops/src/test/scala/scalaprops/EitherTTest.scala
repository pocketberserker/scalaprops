package scalaprops

import scalaz._
import scalaz.std.anyVal._

object EitherTTest extends Scalaprops {

  properties("EitherTMaybe") =
    Properties.list(
      scalazlaws.equal.all[EitherT[Maybe, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[Maybe, Int, a]})#l],
      scalazlaws.monadError.all[({type l[a, b] = EitherT[Maybe, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = EitherT[Maybe, Int, a]})#l]
    )

  properties("EitherTMaybe2") =
    scalazlaws.bitraverse.all[({type l[a, b] = EitherT[Maybe, a, b]})#l]

  properties("EitherTIList") =
    Properties.list(
      scalazlaws.equal.all[EitherT[IList, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[IList, Int, a]})#l],
      scalazlaws.monadError.all[({type l[a, b] = EitherT[IList, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = EitherT[IList, Int, a]})#l]
    )

  properties("EitherTNel") =
    Properties.list(
      scalazlaws.equal.all[EitherT[NonEmptyList, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l],
      scalazlaws.monadError.all[({type l[a, b] = EitherT[NonEmptyList, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l]
    )

  properties("Monad Trans") = scalazlaws.monadTrans.all[({type l[f[_], a] = EitherT[f, Int, a]})#l]

}
