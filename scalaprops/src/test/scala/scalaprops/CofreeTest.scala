package scalaprops

import scalaz._
import scalaz.std.anyVal._

object CofreeTest extends Scalaprops {

  private[this] implicit def cogenCofree[F[_], A](implicit
    A: Cogen[A],
    F: shapeless.Lazy[Cogen[F[Cofree[F, A]]]]
  ): Cogen[Cofree[F, A]] =
    new Cogen[Cofree[F, A]] {
      override def cogen[B](a: Cofree[F, A], g: CogenState[B]) =
        A.cogen(a.head, F.value.cogen(a.tail, g))
    }

  private[this] implicit def cofreeEqual[F[_], A](implicit
    F: shapeless.Lazy[Equal[F[Cofree[F, A]]]],
    A: Equal[A]
  ): Equal[Cofree[F, A]] =
    Equal.equal((a, b) =>
      A.equal(a.head, b.head) && F.value.equal(a.tail, b.tail)
    )

  properties("Maybe") = {
    implicit def genCofreeMaybe[A: Gen] =
      Gen[OneAnd[List, A]].map{ list =>
        Cofree.unfold(list){
          case OneAnd(a, h :: t) =>
            (a, Maybe.just(OneAnd(h, t)))
          case OneAnd(a, Nil) =>
            (a, Maybe.empty[OneAnd[List, A]])
        }
      }

    type F[A] = Cofree[Maybe, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  properties("stream") = {
    type CofreeStream[A] = Cofree[Stream, A]

    import scalaz.std.stream._
    import scalaz.Isomorphism._

    val iso: Tree <~> CofreeStream =
      new IsoFunctorTemplate[Tree, CofreeStream] {
        def to[A](tree: Tree[A]) =
          Cofree(tree.rootLabel, tree.subForest.map(to))
        def from[A](c: CofreeStream[A]) =
          Tree.node(c.head, c.tail.map(from(_)))
      }

    implicit def gen[A: Gen]: Gen[CofreeStream[A]] =
      Gen[Tree[A]].map(iso.to)

    Properties.list(
      scalazlaws.monad.all[CofreeStream],
      scalazlaws.comonad.all[CofreeStream],
      scalazlaws.traverse1.all[CofreeStream],
      scalazlaws.equal.all[CofreeStream[Int]]
    )
  }

  private[this] object CofreeGenImplicit {
    implicit def gen[F[_], A](implicit
      F: shapeless.Lazy[Gen[F[Cofree[F, A]]]],
      A: Gen[A]
    ): Gen[Cofree[F, A]] =
      Apply[Gen].apply2(A, F.value)((h, t) =>
        Cofree(h, t)
      )
  }

  properties("disjunction") = {
    type E[A] = Byte \/ A
    type F[A] = Cofree[E, A]

    import CofreeGenImplicit._

    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  properties("validation") = {
    type E[A] = ValidationNel[Byte, A]
    type F[A] = Cofree[E, A]

    import CofreeGenImplicit._

    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }
}
