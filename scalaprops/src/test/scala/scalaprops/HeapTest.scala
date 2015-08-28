package scalaprops

import scalaz.Heap
import scalaz.std.anyVal._

object HeapTest extends Scalaprops {

  properties("Monoid") = scalazlaws.monoid.all[Heap[Int]]

  properties("Equal") = scalazlaws.equal.all[Heap[Int]]

  properties("Foldable") = scalazlaws.foldable.all[Heap]

}
