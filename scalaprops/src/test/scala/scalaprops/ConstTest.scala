package scalaprops

import scalaz._
import scalaz.std.anyVal._

object ConstTest extends Scalaprops {

  properties("Int")= {
    type F[A] = Const[Int, A]
    Properties.list(
      scalazlaws.applicative.all[F],
      // since 7.1.3
      //scalazlaws.contravariant.all[F],
      scalazlaws.order.all[Const[Int, Int]]
    )
  }

}
