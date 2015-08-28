package scalaprops

import scalaz._
import scalaz.std.anyVal._

object NullArgumentTest extends Scalaprops {

  import FunctionEqual._

  private[this] implicit def equal[A, B](implicit A: Equal[Option[A] => B]): Equal[NullArgument[A, B]] =
    A.contramap(_.apply)

  properties("Compose") = scalazlaws.compose.all[NullArgument]
  properties("Monad") = scalazlaws.monad.all[({type l[a] = NullArgument[Int, a]})#l]
  properties("Contravariant") = scalazlaws.contravariant.all[({type l[a] = NullArgument[a, Int]})#l]
  properties("Monoid") = scalazlaws.monoid.all[NullArgument[Int, Int]]
}
