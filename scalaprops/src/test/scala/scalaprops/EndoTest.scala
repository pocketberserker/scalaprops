package scalaprops

import scalaz.{Equal, Endo}
import scalaz.std.anyVal._

object EndoTest extends Scalaprops {

  private[this] implicit def endoEqual[A: Gen: Equal]: Equal[Endo[A]] = {
    import FunctionEqual._
    Equal[A => A].contramap(_.run)
  }

  properties("InvariantFunctor") = scalazlaws.invariantFunctor.all[Endo]
  properties("Monoid") = scalazlaws.monoid.all[Endo[Int]]

}
