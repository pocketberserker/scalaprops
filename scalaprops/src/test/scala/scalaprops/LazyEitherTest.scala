package scalaprops

import scalaz.{Equal, LazyEither}
import scalaz.std.anyVal._
import scalaz.std.either._

object LazyEitherTest extends Scalaprops {

  implicit def lazyEitherEqual[A: Equal, B: Equal]: Equal[LazyEither[A, B]] =
    Equal[A Either B].contramap(_.toEither)

  properties("Laws1") = Properties.list(
    scalazlaws.monadError.all[LazyEither, Int],
    scalazlaws.traverse.all[({type l[a] = LazyEither[Int, a]})#l]
  )

  properties("Law2") = scalazlaws.bitraverse.all[LazyEither]
  properties("Law3") = scalazlaws.associative.all[LazyEither]

}
