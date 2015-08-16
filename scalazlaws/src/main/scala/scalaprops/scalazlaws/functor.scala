package scalaprops
package scalazlaws

import scalaprops.Property.{forAll, forAllGS}
import scalaprops.Properties.properties
import scalaz._

sealed abstract class functor[F[_, _], G[_]] {

  def identity[M[_], X](implicit M: Functor[M], afx: G[M[X]], ef: Equal[M[X]]): Property

  def composite[M[_], X, Y, Z](implicit M: Functor[M], af: G[M[X]], axy: G[F[X, Y]],
                               ayz: G[F[Y, Z]], ef: Equal[M[Z]]): Property

  final def laws[M[_]](implicit M: Functor[M], af: G[M[Int]], axy: G[F[Int, Int]],
                 ef: Equal[M[Int]]): Properties[ScalazLaw] =
    properties(ScalazLaw.functor)(
      ScalazLaw.functorIdentity -> identity[M, Int],
      ScalazLaw.functorCompsite -> composite[M, Int, Int, Int]
    )

  def all[M[_]](implicit M: Functor[M], af: G[M[Int]], axy: G[F[Int, Int]],
                ef: Equal[M[Int]]): Properties[ScalazLaw]
}

object functorS extends functor[Fun, GS] {

  def identity[M[_], X](implicit M: Functor[M], afx: GS[M[X]], ef: Equal[M[X]]) =
    forAllGS(M.functorLaw.identity[X] _)

  def composite[M[_], X, Y, Z](implicit M: Functor[M], af: GS[M[X]], axy: GS[Fun[X, Y]],
                               ayz: GS[Fun[Y, Z]], ef: Equal[M[Z]]) =
    forAllGS{ (ma: M[X], f1: Fun[X, Y], f2: Fun[Y, Z]) =>
      M.functorLaw.composite(ma, f1.fun, f2.fun)
    }

  def all[M[_]](implicit M: Functor[M], af: GS[M[Int]], axy: GS[Fun[Int, Int]],
                ef: Equal[M[Int]]) =
    Properties.fromProps(ScalazLaw.functorAll, laws[M], invariantFunctorS.laws[M])
}

object functor extends functor[Function1, Gen] {

  def identity[M[_], X](implicit M: Functor[M], afx: Gen[M[X]], ef: Equal[M[X]]) =
    forAll(M.functorLaw.identity[X] _)

  def composite[M[_], X, Y, Z](implicit M: Functor[M], af: Gen[M[X]], axy: Gen[X => Y],
                               ayz: Gen[Y => Z], ef: Equal[M[Z]]) =
    forAll(M.functorLaw.composite[X, Y, Z] _)

  def all[M[_]](implicit M: Functor[M], af: Gen[M[Int]], axy: Gen[Int => Int],
                ef: Equal[M[Int]]) =
    Properties.fromProps(ScalazLaw.functorAll, laws[M], invariantFunctor.laws[M])
}
