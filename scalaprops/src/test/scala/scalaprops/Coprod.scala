package scalaprops

import scalaprops.Coprod._
import scalaz._

trait Triple[F[_]] extends Monad[F] {
  def etaInv[A](fa: F[A]): Maybe[A]
}

/**
 * @see [[http://www.informatik.uni-bremen.de/~cxl/papers/icfp02.pdf Composing Monads Using Coproducts]]
 */
sealed abstract class Coprod[F[_], G[_], A] extends Product with Serializable {

  final def map[B](f: A => B)(implicit F: Functor[F], G: Functor[G]): Coprod[F, G, B] =
    this match {
      case T1(run) =>
        T1(F.map(run)(_.map(f)))
      case T2(run) =>
        T2(G.map(run)(_.map(f)))
      case Var(run) =>
        Var(f(run))
    }

  final def fold[B](f: F[B] => B, g: G[B] => B, a: A => B)(implicit F: Functor[F], G: Functor[G]): B =
    this match {
      case T1(run) =>
        f(F.map(run)(_.fold(f, g, a)))
      case T2(run) =>
        g(G.map(run)(_.fold(f, g, a)))
      case Var(run) =>
        a(run)
    }

  final def strip(implicit F: Triple[F], G: Triple[G]): Coprod[F, G, A] =
    fold[Coprod[F, G, A]](strip1, strip2, Var.apply)

  final def lift1(implicit F: Applicative[F]): F[Coprod[F, G, A]] =
    this match {
      case T1(run) =>
        run
      case _ =>
        F.point(this)
    }

  final def lift2(implicit G: Applicative[G]): G[Coprod[F, G, A]] =
    this match {
      case T2(run) =>
        run
      case _ =>
        G.point(this)
    }

  final def wit(implicit F: Triple[F], G: Triple[G]): Coprod[F, G, A] =
    fold[Coprod[F, G, A]](wit1, wit2, Var(_))

  final def etaInv: Maybe[A] =
    this match {
      case Var(run) =>
        Maybe.Just(run)
      case _ =>
        Maybe.empty[A]
    }
}

object Coprod {

  final case class T1[F[_], G[_], A](run: F[Coprod[F, G, A]]) extends Coprod[F, G, A]
  final case class T2[F[_], G[_], A](run: G[Coprod[F, G, A]]) extends Coprod[F, G, A]
  final case class Var[F[_], G[_], A](run: A) extends Coprod[F, G, A]

  implicit def coprodGen[F[_], G[_], A](implicit
    F: Gen1[F],
    G: Gen1[G],
    A: Gen[A]
  ): Gen[Coprod[F, G, A]] =
    Gen.oneOfLazy(
      Need(A.map(Var(_))),
      Need(F.gen1(coprodGen[F, G, A]).map(T1(_))),
      Need(G.gen1(coprodGen[F, G, A]).map(T2(_)))
    )

  implicit def coprodEqual[F[_], G[_], A](implicit
    F: shapeless.Lazy[Equal[F[Coprod[F, G, A]]]],
    G: shapeless.Lazy[Equal[G[Coprod[F, G, A]]]],
    A: Equal[A],
    TF: Triple[F],
    TG: Triple[G]
  ): Equal[Coprod[F, G, A]] =
    Equal.equal[Coprod[F, G, A]]{
      case (T1(x), T1(y)) =>
        F.value.equal(x, y)
      case (T2(x), T2(y)) =>
        G.value.equal(x, y)
      case (Var(x), Var(y)) =>
        A.equal(x, y)
      case _ =>
        false
    }.contramap(_.strip)

  implicit def coprodTriple[F[_], G[_]](implicit F: Triple[F], G: Triple[G]): Triple[({type l[a] = Coprod[F, G, a]})#l] =
    new Triple[({type l[a] = Coprod[F, G, a]})#l] {
      override def point[A](a: => A) =
        Var(a)

      override def map[A, B](fa: Coprod[F, G, A])(f: A => B) =
        fa map f

      override def bind[A, B](fa: Coprod[F, G, A])(f: A => Coprod[F, G, B]) =
        join(map(fa)(f))

      override def join[A](fa: Coprod[F, G, Coprod[F, G, A]]) =
        fa.fold[Coprod[F, G, A]](wit1, wit2, identity)

      override def etaInv[A](fa: Coprod[F, G, A]) =
        fa.etaInv
    }

  def strip1[F[_], G[_], A](c: F[Coprod[F, G, A]])(implicit F: Triple[F]): Coprod[F, G, A] =
    F.etaInv(c) match {
      case Maybe.Just(a) =>
        a
      case Maybe.Empty() =>
        T1(c)
    }

  def strip2[F[_], G[_], A](c: G[Coprod[F, G, A]])(implicit G: Triple[G]): Coprod[F, G, A] =
    G.etaInv(c) match {
      case Maybe.Just(a) =>
        a
      case Maybe.Empty() =>
        T2(c)
    }


  def wit1[F[_], G[_], A](t: F[Coprod[F, G, A]])(implicit F: Triple[F]): Coprod[F, G, A] =
    strip1(F.join(F.map(t)(_.lift1)))

  def wit2[F[_], G[_], A](t: G[Coprod[F, G, A]])(implicit G: Triple[G]): Coprod[F, G, A] =
    strip2(G.join(G.map(t)(_.lift2)))

}
