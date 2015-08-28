package scalaprops

import scalaz.EphemeralStream
import scalaz.std.anyVal._

object EphemeralStreamTest extends Scalaprops {

  properties("laws") = Properties.list(
    scalazlaws.monadPlusStrong.all[EphemeralStream],
    scalazlaws.traverse.all[EphemeralStream],
    scalazlaws.cobind.all[EphemeralStream],
    // since 7.1.3
    //scalazlaws.isEmpty.all[EphemeralStream],
    scalazlaws.align.all[EphemeralStream],
    scalazlaws.zip.all[EphemeralStream]
  )

}
