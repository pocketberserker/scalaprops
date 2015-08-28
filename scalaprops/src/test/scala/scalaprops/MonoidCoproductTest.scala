package scalaprops

import scalaz._
import scalaz.std.anyVal._

object MonoidCoproductTest extends Scalaprops {

  properties("Laws") =
    Properties.list(
      scalazlaws.equal.all[Int :+: Int],
      scalazlaws.monoid.all[Int :+: Int]
    )

}
