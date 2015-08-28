package scalaprops

import scalaz._
import scalaz.std.anyVal._

object TheseTest extends Scalaprops {

  private type F[A] = Int \&/ A

  properties("test0") =
    Properties.list(
      scalazlaws.equal.all[Int \&/ Int],
      scalazlaws.semigroup.all[Int \&/ Int]
    )

  properties("test1") =
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.cobind.all[F],
      scalazlaws.traverse.all[F]
    )

  properties("test2") =
    scalazlaws.bitraverse.all[\&/]
}
