package scalaprops
package scalazlaws

import scalaprops.Property.{forAll, forAllGS}
import scalaz._

sealed abstract class apply[F[_, _], G[_]] {
  def composition[M[_], X, Y, Z](implicit ap: Apply[M], afx: G[M[X]], au: G[M[F[Y, Z]]],
                                 av: G[M[F[X, Y]]], e: Equal[M[Z]]): Property

  final def laws[M[_]](implicit M: Apply[M], af: G[M[Int]],
                 aff: G[M[F[Int, Int]]], e: Equal[M[Int]]): Properties[ScalazLaw] =
    Properties.fromChecks(ScalazLaw.apply)(
      ScalazLaw.applyComposition -> Check(
        composition[M, Int, Int, Int], Param.maxSize(5)
      )
    )

  def all[M[_]](implicit M: Apply[M], af: G[M[Int]],
                aff: G[M[F[Int, Int]]], e: Equal[M[Int]]): Properties[ScalazLaw]
}

object applyS extends apply[Fun, GS] {
  def composition[M[_], X, Y, Z](implicit M: Apply[M], afx: GS[M[X]], au: GS[M[Fun[Y, Z]]],
                                 av: GS[M[Fun[X, Y]]], e: Equal[M[Z]]) =
    forAllGS{ (f1: M[Fun[Y, Z]], f2: M[Fun[X, Y]], ma: M[X]) =>
      M.applyLaw.composition(
        M.map(f1)(_.fun), M.map(f2)(_.fun), ma
      )
    }

  def all[M[_]](implicit M: Apply[M], af: GS[M[Int]],
                aff: GS[M[Fun[Int, Int]]], e: Equal[M[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.applyAll, laws[M], functorS.all[M])
}

object apply extends apply[Function1, Gen] {
  def composition[M[_], X, Y, Z](implicit M: Apply[M], afx: Gen[M[X]], au: Gen[M[Y => Z]],
                                 av: Gen[M[X => Y]], e: Equal[M[Z]]) =
    forAll(M.applyLaw.composition[X, Y, Z] _)

  def all[M[_]](implicit M: Apply[M], af: Gen[M[Int]],
                aff: Gen[M[Int => Int]], e: Equal[M[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.applyAll, apply.laws[M], functor.all[M])
}
