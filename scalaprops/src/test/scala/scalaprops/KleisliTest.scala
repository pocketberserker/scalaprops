package scalaprops

import scalaz._
import scalaz.std.tuple._
import scalaz.std.anyVal._

object KleisliTest extends Scalaprops {

  private[this] val e = new FunctionEqual(3)

  implicit def kleisliEqual[F[_], A: Gen, B](implicit E: Equal[F[B]]): Equal[Kleisli[F, A, B]] = {
    import e._
    Equal[A => F[B]].contramap(_.run)
  }

  private def kleisliTest[F[_]: Monad](implicit
    F: Equal[F[Int]],
    E1: Equal[F[(Int, Int)]],
    E2: Equal[F[(Int, (Int, Int))]],
    E3: Equal[F[((Int, Int), Int)]],
    G1: Gen[Kleisli[F, Int, Int]],
    G2: Gen[Kleisli[F, Int, Int => Int]]
  ) = {
    type K1[a] = Kleisli[F, Int, a]
    type K2[a, b] = Kleisli[F, a, b]

    Properties.list(
      scalazlaws.monad.all[K1],
      scalazlaws.arrow.all[K2]
    )
  }

  private val sizeSetting = Foldable1[NonEmptyList].foldLeft1(
    NonEmptyList(
      ScalazLaw.bindApConsistentWithBind,
      ScalazLaw.composeAssociative,
      ScalazLaw.plusAssociative,
      ScalazLaw.semigroupAssociative
    ).map { law =>
      { case `law` => Param.maxSize(30) }: PartialFunction[ScalazLaw, Endo[Param]]
    }
  )(_ orElse _)


  properties("Maybe") = kleisliTest[Maybe]

  properties("IList") = kleisliTest[IList].andThenParamPF{
    case Or.R(Or.L(p)) if sizeSetting.isDefinedAt(p) => sizeSetting(p)
  }

  properties("NonEmptyList") = kleisliTest[NonEmptyList].andThenParamPF{
    case Or.R(Or.L(p)) if sizeSetting.isDefinedAt(p) => sizeSetting(p)
  }

  properties("monadTrans") = scalazlaws.monadTrans.all[({type l[f[_], a] = Kleisli[f, Int, a]})#l]

}
