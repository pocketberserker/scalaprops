package scalaprops

import scalaz._
import scalaz.std.anyVal._

object DequeueTest extends Scalaprops {

  properties("Laws") =
    Properties.list(
      scalazlaws.functor.all[Dequeue],
      scalazlaws.foldable.all[Dequeue],
      scalazlaws.isEmpty.all[Dequeue]
    )

}
