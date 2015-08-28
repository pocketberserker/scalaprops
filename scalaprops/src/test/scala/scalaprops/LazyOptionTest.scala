package scalaprops

import scalaz.std.anyVal._
import scalaz.LazyOption

object LazyOptionTest extends Scalaprops {

  properties("laws") = Properties.list(
    scalazlaws.monadPlusStrong.all[LazyOption],
    scalazlaws.traverse.all[LazyOption],
    scalazlaws.zip.all[LazyOption],
    // since 7.1.3
    //scalazlaws.isEmpty.all[LazyOption],
    scalazlaws.cobind.all[LazyOption],
    scalazlaws.align.all[LazyOption]
  )

}
