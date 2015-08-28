package scalaprops

import scalaz.std.set._
import scalaz.std.anyVal._

object SetTest extends Scalaprops {

  properties("Laws") =
    Properties.list(
      scalazlaws.order.all[Set[Int]],
      scalazlaws.isEmpty.all[Set],
      scalazlaws.foldable.all[Set]
    )

}
