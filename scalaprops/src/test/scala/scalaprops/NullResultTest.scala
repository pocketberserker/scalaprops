package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.option._

object NullResultTest extends Scalaprops {

  import FunctionEqual._

  private[this] implicit def equal[A, B](implicit A: Equal[A => Option[B]]): Equal[NullResult[A, B]] =
    A.contramap(_.apply)

  properties("Category") = scalazlaws.category.all[NullResult]
  properties("Monad") = scalazlaws.monad.all[({type l[a] = NullResult[Int, a]})#l]
  properties("Contravariant") = scalazlaws.contravariant.all[({type l[a] = NullResult[a, Int]})#l]
  properties("Monoid") = scalazlaws.monoid.all[NullResult[Int, Int]]

}
