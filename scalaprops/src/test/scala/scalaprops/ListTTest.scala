package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.list._

object ListTTest extends Scalaprops {

  properties("Maybe") =
    scalazlaws.monad.all[({type l[a] = ListT[Maybe, a]})#l]

  properties("DisableTestList") =
    scalazlaws.bind.laws[({type l[a] = ListT[IList, a]})#l].andThenParam(Param.maxSize(2))
      .ignore("https://github.com/scalaz/scalaz/issues/921")

  properties("monadTrans") = scalazlaws.monadTrans.all[ListT]

}
