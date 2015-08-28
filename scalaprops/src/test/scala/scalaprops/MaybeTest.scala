package scalaprops

import scalaz._
import scalaz.std.anyVal._

object MaybeTest extends Scalaprops {

  properties("Laws") = Properties.list(
    scalazlaws.monadPlusStrong.all[Maybe],
    scalazlaws.traverse.all[Maybe],
    scalazlaws.zip.all[Maybe]
  )
}
