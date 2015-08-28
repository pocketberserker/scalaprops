package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object LensTest extends Scalaprops{

  import FunctionEqual._
  import IndexedStoreTTest.indexedStoreTEqual

  private[this] implicit def lensEqual[A, B, C, D](implicit A: Equal[A => IndexedStore[C, D, B]]): Equal[LensFamily[A, B, C, D]] =
    A.contramap[LensFamily[A, B, C, D]](_.run)

  properties("CategoryLaw") = {
    implicit val l = Gen.value(Lens.lensId[Int])
    scalazlaws.category.all[Lens]
  }

  properties("Id") = scalazlaws.lens.all(Lens.lensId[Int])
  properties("Trivial") = scalazlaws.lens.all(Lens.trivialLens[Int])
  properties("Codiag") = scalazlaws.lens.all(Lens.codiagLens[Int])
  properties("First") = scalazlaws.lens.all(Lens.firstLens[Int, Int])
  properties("Second") = scalazlaws.lens.all(Lens.secondLens[Int, Int])

}
