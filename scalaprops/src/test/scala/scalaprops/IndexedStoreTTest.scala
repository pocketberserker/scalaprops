package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object IndexedStoreTTest extends Scalaprops {

  private[this] val e = new FunctionEqual(10)
  import e._

  implicit def indexedStoreTEqual[F[_], I: Equal, A, B](implicit F: Equal[F[A => B]]): Equal[IndexedStoreT[F, I, A, B]] =
    Equal[(F[A => B], I)].contramap(_.run)


  properties("Maybe1") = scalazlaws.contravariant.all[({type l[A] = IndexedStoreT[Maybe, Int, A, Int]})#l]
  properties("IList1") = scalazlaws.contravariant.all[({type l[A] = IndexedStoreT[IList, Int, A, Int]})#l]

  properties("Maybe2") = scalazlaws.bifunctor.all[({type l[A, B] = IndexedStoreT[Maybe, A, Int, B]})#l]
  properties("IList2") = scalazlaws.bifunctor.all[({type l[A, B] = IndexedStoreT[IList, A, Int, B]})#l]

  properties("Nel") = scalazlaws.comonad.all[({type l[A] = StoreT[NonEmptyList, Int, A]})#l]
  properties("Maybe3") = scalazlaws.cobind.all[({type l[A] = StoreT[Maybe, Int, A]})#l]
  properties("IList3") = scalazlaws.cobind.all[({type l[A] = StoreT[IList, Int, A]})#l]

  properties("comonadTransLaws") = scalazlaws.comonadTrans.all[({type l[f[_], a] = StoreT[f, Int, a]})#l]
}
