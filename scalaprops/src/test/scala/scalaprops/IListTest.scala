package scalaprops

import scalaprops.Property.forAllG
import scalaz.IList
import scalaz.std.anyVal._

object IListTest extends Scalaprops {

  properties("Laws") = Properties.list(
    scalazlaws.order.all[IList[Byte]],
    scalazlaws.monadPlusStrong.all[IList],
    scalazlaws.traverse.all[IList],
    scalazlaws.cobind.all[IList],
    scalazlaws.isEmpty.all[IList],
    scalazlaws.align.all[IList],
    scalazlaws.zip.all[IList]
  )

  properties("listOf") = {
    val g = for {
      min <- Gen.choose(-10, 50)
      xs <- Gen.listOf(Gen[Byte], min)
    } yield (min, xs)

    val p = forAllG(g) {
      case (min, xs) => xs.length >= min
    }
    p.toProperties("minimum length")
  }

  property("listOf1") = forAllG(Gen.listOf1(Gen[Byte]))(_.nonEmpty)

}
