package scalaprops
package scalazlaws

import scalaprops.Property.{forAll, forAllGS}
import scalaprops.Properties.properties
import scalaz._

sealed abstract class monad[F[_, _], G[_]] {
  def rightIdentity[M[_], X](implicit M: Monad[M], e: Equal[M[X]], a: G[M[X]]): Property

  def leftIdentity[M[_], X, Y](implicit M: Monad[M], emy: Equal[M[Y]], ax: G[X], af: G[F[X, M[Y]]]): Property

  final def laws[M[_]: Monad](implicit g: G[Int], am: G[M[Int]],
                 af: G[F[Int, M[Int]]], ag: G[M[F[Int, Int]]], e: Equal[M[Int]]) =
    properties(ScalazLaw.monad)(
      ScalazLaw.monadRightIdentity -> this.rightIdentity[M, Int],
      ScalazLaw.monadLeftIdentity -> this.leftIdentity[M, Int, Int]
    )

  def all[M[_]](implicit a: Monad[M], am: G[M[Int]],
                af: G[F[Int, M[Int]]], ag: G[M[F[Int, Int]]], e: Equal[M[Int]]): Properties[ScalazLaw]
}

object monadS extends monad[Fun, GS] {
  def rightIdentity[M[_], X](implicit M: Monad[M], e: Equal[M[X]], a: GS[M[X]]): Property =
    forAllGS(M.monadLaw.rightIdentity[X] _)

  def leftIdentity[M[_], X, Y](implicit M: Monad[M], emy: Equal[M[Y]], ax: GS[X], af: GS[Fun[X, M[Y]]]): Property =
    forAllGS{ (a: X, f: Fun[X, M[Y]]) =>
      M.monadLaw.leftIdentity(a, f.fun)
    }

  def all[M[_]](implicit a: Monad[M], am: GS[M[Int]], af: GS[Fun[Int, M[Int]]], ag: GS[M[Fun[Int, Int]]], e: Equal[M[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.monadAll, this.laws[M], bindS.all[M], applicativeS.all[M])
}

object monad extends monad[Function1, Gen] {
  def rightIdentity[M[_], X](implicit M: Monad[M], e: Equal[M[X]], a: Gen[M[X]]) =
    forAll(M.monadLaw.rightIdentity[X] _)

  def leftIdentity[M[_], X, Y](implicit am: Monad[M], emy: Equal[M[Y]], ax: Gen[X], af: Gen[X => M[Y]]) =
    forAll(am.monadLaw.leftIdentity[X, Y] _)

  def all[M[_]](implicit a: Monad[M], am: Gen[M[Int]],
                af: Gen[Int => M[Int]], ag: Gen[M[Int => Int]], e: Equal[M[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.monadAll, monad.laws[M], bind.all[M], applicative.all[M])
}
