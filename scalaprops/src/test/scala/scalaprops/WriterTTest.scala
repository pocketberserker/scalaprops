package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object WriterTTest extends Scalaprops {

  properties("Maybe1") = {
    type F[A] = WriterT[Maybe, Int, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  properties("Maybe2") = {
    type F[A, B] = WriterT[Maybe, A, B]

    scalazlaws.bitraverse.all[F]
  }

  properties("iList1") = {
    type F[A] = WriterT[IList, Int, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  properties("either") = {
    type F[A] = Byte \/ A
    type G[A] = WriterT[F, Short, A]

    Properties.list(
      scalazlaws.monad.all[G],
      scalazlaws.traverse.all[G],
      scalazlaws.equal.all[G[Int]]
    )
  }

  properties("id") = {
    type F[A] = Writer[Int, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  properties("monadTrans") = scalazlaws.monadTrans.all[({type l[f[_], a] = WriterT[f, Int, a]})#l]

}
