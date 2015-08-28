package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.stream._

object StreamTTest extends Scalaprops {

  properties("Id") = {
    import scalaz.Id._
    type F[A] = StreamT[Id, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  properties("OneOrTwo") = {
    type OneOrTwo[A] = OneAnd[Maybe, A]
    type F[A] = StreamT[OneOrTwo, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }.andThenParam(Param.maxSize(2))

  properties("Maybe") = {
    type F[A] = StreamT[Maybe, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  properties("iList") = {
    type F[A] = StreamT[IList, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }.andThenParam(Param.maxSize(3))

  properties("monadTrans") = scalazlaws.monadTrans.all[StreamT]

}
