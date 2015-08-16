package scalaprops
package scalazlaws

import scalaprops.Property.{forAll, forAllGS}
import scalaprops.Properties.properties
import scalaz._

sealed abstract class applicative[F[_, _], G[_]] {
  def identity[M[_], X](implicit f: Applicative[M], afx: G[M[X]], ef: Equal[M[X]]): Property

  def homomorphism[M[_], X, Y](implicit ap: Applicative[M], ax: G[X], af: G[F[X, Y]], e: Equal[M[Y]]): Property

  def interchange[M[_], X, Y](implicit ap: Applicative[M], ax: G[X], afx: G[M[F[X, Y]]], e: Equal[M[Y]]): Property

  def mapApConsistency[M[_], X, Y](implicit ap: Applicative[M], ax: G[M[X]], afx: G[F[X, Y]], e: Equal[M[Y]]): Property

  final def laws[M[_]](implicit M: Applicative[M], a1: G[Int], a2: G[F[Int, Int]],
                       af: G[M[Int]], aff: G[M[F[Int, Int]]],
                       e: Equal[M[Int]]) = properties(ScalazLaw.applicative)(
    ScalazLaw.applicativeIdentity -> this.identity[M, Int],
    ScalazLaw.applicativeHomomorphism -> this.homomorphism[M, Int, Int],
    ScalazLaw.applicativeInterchange -> this.interchange[M, Int, Int],
    ScalazLaw.applicativeMapConsistentWithAp -> this.mapApConsistency[M, Int, Int]
  )

  def all[M[_]](implicit M: Applicative[M], af: G[M[Int]],
                aff: G[M[F[Int, Int]]], e: Equal[M[Int]]): Properties[ScalazLaw]
}

object applicativeS extends applicative[Fun, GS] {
  def identity[M[_], X](implicit f: Applicative[M], afx: GS[M[X]], ef: Equal[M[X]]): Property =
    forAllGS(f.applicativeLaw.identityAp[X] _)

  def homomorphism[M[_], X, Y](implicit ap: Applicative[M], ax: GS[X], af: GS[Fun[X, Y]], e: Equal[M[Y]]): Property =
    forAllGS{ (f: Fun[X, Y], a: X) =>
      ap.applicativeLaw.homomorphism(f.fun, a)
    }

  def interchange[M[_], X, Y](implicit ap: Applicative[M], ax: GS[X], afx: GS[M[Fun[X, Y]]], e: Equal[M[Y]]): Property =
    forAllGS{ (f: M[Fun[X, Y]], a: X) =>
      ap.applicativeLaw.interchange(ap.map(f)(_.fun), a)
    }

  def mapApConsistency[M[_], X, Y](implicit ap: Applicative[M], ax: GS[M[X]], afx: GS[Fun[X, Y]], e: Equal[M[Y]]): Property =
    forAllGS{ (f: Fun[X, Y], a: M[X]) =>
      ap.applicativeLaw.mapLikeDerived(f.fun, a)
    }

  def all[M[_]](implicit M: Applicative[M], af: GS[M[Int]], aff: GS[M[Fun[Int, Int]]], e: Equal[M[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.applicativeAll, this.laws[M], applyS.all[M])
}

object applicative extends applicative[Function1, Gen] {
  def identity[M[_], X](implicit f: Applicative[M], afx: Gen[M[X]], ef: Equal[M[X]]) =
    forAll(f.applicativeLaw.identityAp[X] _)

  def homomorphism[M[_], X, Y](implicit ap: Applicative[M], ax: Gen[X], af: Gen[X => Y], e: Equal[M[Y]]) =
    forAll(ap.applicativeLaw.homomorphism[X, Y] _)

  def interchange[M[_], X, Y](implicit ap: Applicative[M], ax: Gen[X], afx: Gen[M[X => Y]], e: Equal[M[Y]]) =
    forAll(ap.applicativeLaw.interchange[X, Y] _)

  def mapApConsistency[M[_], X, Y](implicit ap: Applicative[M], ax: Gen[M[X]], afx: Gen[X => Y], e: Equal[M[Y]]) =
    forAll(ap.applicativeLaw.mapLikeDerived[X, Y] _)

  def all[M[_]](implicit M: Applicative[M], af: Gen[M[Int]],
                aff: Gen[M[Int => Int]], e: Equal[M[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.applicativeAll, this.laws[M], apply.all[M])
}
