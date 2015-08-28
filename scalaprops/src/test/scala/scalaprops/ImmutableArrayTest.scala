package scalaprops

import scalaz.ImmutableArray
import scalaz.std.anyVal._

object ImmutableArrayTest extends Scalaprops {

  properties("Equal") = scalazlaws.equal.all[ImmutableArray[Int]]
  properties("Foldable") = scalazlaws.foldable.all[ImmutableArray]

}
