package scalaprops

import scalaz._
import scalaz.std.anyVal._

object IndexedContsTTest extends Scalaprops {

  private[this] val F = new FunctionEqual(5)
  import F._

  private[this] implicit def indexedContsTEqual[W[_], M[_], R, O, A](implicit F: Equal[W[A => M[O]] => M[R]]): Equal[IndexedContsT[W, M, R, O, A]] =
    F.contramap(_.run)

  properties("ContsTMaybe") = {
    type F[A] = ContsT[NonEmptyList, Maybe, Int, A]
    scalazlaws.monad.all[F].andThenParam(Param.maxSize(4))
  }

  properties("ContsTIList") = {
    type F[A] = ContsT[NonEmptyList, IList, Int, A]
    scalazlaws.monad.all[F]
  }

  properties("Cont") = {
    type F[A] = Conts[NonEmptyList, Int, A]
    scalazlaws.monad.all[F]
  }

  properties("BifunctorMaybeMaybe") =
    scalazlaws.bifunctor.all[({type F[A, B] = IndexedContsT[Maybe, IList, A, Int, B]})#F]

  properties("BifunctorMaybeIList") =
    scalazlaws.bifunctor.all[({type F[A, B] = IndexedContsT[Maybe, IList, A, Int, B]})#F]

  properties("BifunctorIListMaybe") =
    scalazlaws.bifunctor.all[({type F[A, B] = IndexedContsT[IList, Maybe, A, Int, B]})#F]

  properties("ContravariantMaybeMaybe") =
    scalazlaws.contravariant.all[({type F[A] = IndexedContsT[Maybe, IList, Int, A, Int]})#F]

  properties("ContravariantMaybeIList") =
    scalazlaws.contravariant.all[({type F[A] = IndexedContsT[Maybe, IList, Int, A, Int]})#F]

  properties("ContravariantIListMaybe") =
    scalazlaws.contravariant.all[({type F[A] = IndexedContsT[IList, Maybe, Int, A, Int]})#F]
}
