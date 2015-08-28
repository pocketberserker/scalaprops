package scalaprops

import scalaz._
import scalaz.std.tuple._
import scalaz.std.anyVal._

object StateTTest extends Scalaprops {
  private[this] val e = new FunctionEqual(10)
  import e._

  implicit def stateTEqual[F[_], A, B](implicit F: Equal[A => F[(A, B)]]): Equal[StateT[F, A, B]] =
    F.contramap(_.apply _)

  properties("IList") = {
    type F[A] = StateT[IList, Int, A]

    Properties.list(
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  properties("Maybe") = {
    type F[A] = StateT[Maybe, Int, A]

    Properties.list(
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  properties("bifunctor") = scalazlaws.bifunctor.laws[({type l[a, b] = IndexedStateT[Maybe, Int, a, b]})#l]

  properties("Monad Trans") = scalazlaws.monadTrans.all[({type l[f[_], a] = StateT[f, Int, a]})#l]

}
