package scalaprops

import scalaz._
import scalaz.std.anyVal._
import Property.forAll

object ISetTest extends Scalaprops {

  properties("foldable") =
    scalazlaws.foldable.all[ISet]

  properties("order") =
    scalazlaws.order.all[ISet[Int]]

  properties("monoid") =
    scalazlaws.monoid.all[ISet[Int]]

  property("filter") = forAll { (a: ISet[Int], p: Int => Boolean) =>
    (a filter p).toList == a.toList.filter(p)
  }

  property("partition") = forAll { (a: ISet[Int], p: Int => Boolean) =>
    val (x, y) = a partition p
    assert((x.size + y.size) == a.size)
    assert((x union y) == a)
    (x.toList, y.toList) == a.toList.partition(p)
  }

}
