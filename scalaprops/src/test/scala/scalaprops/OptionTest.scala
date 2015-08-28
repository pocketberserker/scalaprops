package scalaprops

import scalaz.std.option._
import scalaz.std.anyVal._

object OptionTest extends Scalaprops {

  properties("Laws") =
    Properties.list(
      scalazlaws.monadPlusStrong.all[Option],
      scalazlaws.traverse.all[Option],
      scalazlaws.zip.all[Option],
      scalazlaws.align.all[Option],
      scalazlaws.isEmpty.all[Option],
      scalazlaws.cobind.all[Option]
    )

}
