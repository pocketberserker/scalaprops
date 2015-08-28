package scalaprops

import scalaz._
import scalaz.std.anyVal._

object LazyOptionTTest extends Scalaprops{

  properties("iList") = Properties.list(
    scalazlaws.equal.all[LazyOptionT[IList, Int]],
    scalazlaws.monad.all[({type l[a] = LazyOptionT[IList, a]})#l]
  )

  properties("maybe") = Properties.list(
    scalazlaws.equal.all[LazyOptionT[Maybe, Int]],
    scalazlaws.monad.all[({type l[a] = LazyOptionT[Maybe, a]})#l]
  )

  properties("Monad Trans") = scalazlaws.monadTrans.all[LazyOptionT]

}
