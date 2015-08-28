package scalaprops

import scalaz.\/
import scalaz.std.anyVal._

object DisjunctionTest extends Scalaprops {

  properties("bitraverse") = scalazlaws.bitraverse.all[\/]
  properties("associative") = scalazlaws.associative.all[\/]
  properties("order") = scalazlaws.order.all[Int \/ Int]

  properties("laws1") = Properties.list(
    scalazlaws.monadError.all[\/, Int],
    scalazlaws.traverse.all[({type l[a] = Int \/ a})#l],
    scalazlaws.plus.all[({type l[a] = Int \/ a})#l]
  )
}
