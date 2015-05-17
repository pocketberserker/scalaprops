package scalaprops

import java.util.concurrent.atomic.AtomicBoolean
import scalaz.Leibniz.===
import scalaz._
import scalaz.Id.Id

final case class PropertyT[F[_]](f: (Int, Rand) => F[(Result, Rand)]) {
  def toCheck: CheckT[F] =
    CheckT(this)

  def toCheckWith(endo: Endo[Param]): CheckT[F] =
    CheckT(this, endo)

  def resize(size: Int): PropertyT[F] =
    Property.fromGen(gen.resize(size))

  def mapSize(g: Int => Int): PropertyT[F] =
    Property.fromGen(gen.mapSize(g))

  def gen: GenT[F, Result] = Gen.genT(f)

  def and(p: PropertyT[F])(implicit F: Bind[F]): PropertyT[F] =
    Property.fromGen(
      Apply[({type l[a] = GenT[F, a]})#l].apply2(gen, p.gen)((res1, res2) =>
        if(res1.isException || res1.isFalsified){
          res1
        }else if(res2.isException || res2.isFalsified){
          res2
        }else if(res1.isProven || res1.isUnfalsified){
          res2
        }else if(res2.isProven || res2.isUnfalsified){
          res1
        }else Result.NoResult
      )
    )

  def or(p: PropertyT[F])(implicit F: Bind[F]): PropertyT[F] =
    Property.fromGen(
      Apply[({type l[a] = GenT[F, a]})#l].apply2(gen, p.gen)((res1, res2) =>
        if(res1.isException || res1.isFalsified){
          res1
        }else if(res2.isException || res2.isFalsified){
          res2
        }else if(res1.isProven || res1.isUnfalsified){
          res1
        }else if(res2.isProven || res2.isUnfalsified){
          res2
        }else Result.NoResult
      )
    )

  def sequence(p: PropertyT[F])(implicit F: Bind[F]): PropertyT[F] =
    Property.fromGen(
      Apply[({type l[a] = GenT[F, a]})#l].apply2(gen, p.gen)((res1, res2) =>
        if(res1.isException || res1.isProven || res1.isUnfalsified) {
          res1
        }else if(res2.isException || res2.isProven || res2.isUnfalsified){
          res2
        }else if(res1.isFalsified){
          res2
        }else if(res2.isFalsified){
          res1
        }else Result.NoResult
      )
    )

  // TODO remove `listener` parameter? use scalaz-stream?
  def check(param: Param, cancel: AtomicBoolean, listener: Int => Unit)(implicit F: F[(Result, Rand)] === (Result, Rand)): CheckResult = {
    import param.{rand => _, _}
    @annotation.tailrec
    def loop(s: Int, discarded: Int, sz: Float, random: Rand): CheckResult = if(cancel.get()) {
      CheckResult.Timeout(s, discarded)
    }else{
      val size = {
        if (s == 0 && discarded == 0) minSize
        else sz + (maxSize - sz) / (minSuccessful - s)
      }

      val r = \/.fromTryCatchThrowable[(Result, Rand), Throwable](
        F(f(math.round(size), random)) // TODO
      )

      r match {
        case \/-((Result.NoResult, nextRand)) =>
          if (discarded + 1 >= maxDiscarded) {
            CheckResult.Exhausted(s, discarded + 1)
          } else {
            loop(s, discarded + 1, size, nextRand)
          }
        case \/-((Result.Proven, _)) =>
          CheckResult.Proven(s + 1, discarded)
        case \/-((Result.Unfalsified(args), nextRand)) =>
          if (s + 1 >= minSuccessful) {
            CheckResult.Passed(s + 1, discarded)
          } else {
            listener(s)
            loop(s + 1, discarded, size, nextRand)
          }
        case \/-((Result.Falsified(args), _)) =>
          CheckResult.Falsified(s, discarded, args)
        case \/-((Result.Exception(args, ex), _)) =>
          CheckResult.PropException(s, discarded, args, ex)
        case \/-((Result.Ignored(reason), _)) =>
          CheckResult.Ignored(s, discarded, reason)
        case -\/(e) =>
          CheckResult.GenException(s, discarded, e)
      }
    }

    loop(0, 0, minSize, param.rand)
  }

  def toProperties[A](id: A, param: Endo[Param] = Param.id): PropertiesT[F, A] =
    Properties.single(id, Check(this, param))

  def ignore[G[_]](reason: String)(implicit G: Applicative[G]): PropertyT[G] =
    PropertyT[G]((_, rand) => G.point((Result.Ignored(reason), rand)))
}

object PropertyT {
  private[this] val noResult = PropertyT[Id]((_, r) => (Result.NoResult, r))

  def of(f: (Int, Rand) => (Result, Rand)): Property =
    new Property(f)

  def implies(b: => Boolean, p: => Property): Property =
    if(b) {
      p
    } else {
      noResult
    }

  def fromGen[F[_]](g: GenT[F, Result]): PropertyT[F] =
    Property(g.f)

  def propFromResultLazy[F[_]](r: Need[Result])(implicit F: Applicative[F]): PropertyT[F] =
    Property((_, rand) => F.point((r.value, rand)))

  def propFromResult[F[_]](r: Result)(implicit F: Applicative[F]): PropertyT[F] =
    Property((_, rand) => F.point((r, rand)))

  val prop: Boolean => Property = b => propFromResult{
    if(b) Result.Proven
    else Result.Falsified(IList.empty)
  }

  private[this] def propLazy(result: Need[Boolean]): Property =
    propFromResultLazy {
      Functor[Need].map(result){ r =>
        if (r) Result.Proven
        else Result.Falsified(IList.empty)
      }
    }

  def forall0[A](g: Gen[A], shrink: Shrink[A])(f: A => Property): Property =
    Property((i, r) => {
      def first(as: Stream[(A, Rand)], shrinks: Int): Maybe[(A, Result, Rand)] = {
        as.map{ case (a, rr) =>
          val x = exception(f(a)).f(i, rr)
          x._1.toMaybe.map(result =>
            (a, result.provenAsUnfalsified.addArg(Arg(a, shrinks)): Result, x._2)
          )
        } match {
          case Stream() =>
            Maybe.empty
          case results @ (h #:: _)=>
            results.find(_.exists(_._2.failed)).getOrElse(h)
        }
      }

      first(Stream(g.f(i, r)), 0) match {
        case Maybe.Just(xx @ (a, re, rand)) if re.failed =>
          @annotation.tailrec
          def loop(shrinks: Int, x: (A, Result, Rand)): (Result, Rand) =
            first(shrink(x._1).map(_ -> rand.next), shrinks) match {
              case Maybe.Just((aa, result, rr)) if result.failed =>
                loop(shrinks + 1, (aa, result, rr.next))
              case _ =>
                (x._2, x._3)
            }
          loop(1, xx)
        case xx =>
          xx.map(t => (t._2, t._3)).getOrElse((Result.NoResult, Rand.standard(0)))
      }
    })

  def exception[F[_]](p: => PropertyT[F])(implicit F: Applicative[F]): PropertyT[F] =
    try {
      p
    } catch {
      case t: Throwable =>
        PropertyT[F]((i, r) => F.point(Result.Exception(IList.empty, t) -> r))
    }

  def forAll(result: => Boolean): Property =
    propLazy(Need(result))

  def forAll[A1](f: A1 => Boolean)(implicit A1: Gen[A1]): Property =
    forall0(A1, Shrink.empty)(f.andThen(prop))

  def forAll[A1, A2](f: (A1, A2) => Boolean)(implicit A1: Gen[A1], A2: Gen[A2]): Property =
    forall0(A1, Shrink.empty)(a1 =>
      forall0(A2, Shrink.empty)(a2 =>
        prop(f(a1, a2))
      )
    )

  def forAll[A1, A2, A3](f: (A1, A2, A3) => Boolean)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3]): Property =
    forall0(A1, Shrink.empty)(a1 =>
      forall0(A2, Shrink.empty)(a2 =>
        forall0(A3, Shrink.empty)(a3 =>
          prop(f(a1, a2, a3))
        )
      )
    )

  def forAll[A1, A2, A3, A4](f: (A1, A2, A3, A4) => Boolean)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4]): Property =
    forall0(A1, Shrink.empty)(a1 =>
      forall0(A2, Shrink.empty)(a2 =>
        forall0(A3, Shrink.empty)(a3 =>
          forall0(A4, Shrink.empty)(a4 =>
            prop(f(a1, a2, a3, a4))
          )
        )
      )
    )

  def forAll[A1, A2, A3, A4, A5](f: (A1, A2, A3, A4, A5) => Boolean)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5]): Property =
    forall0(A1, Shrink.empty)(a1 =>
      forall0(A2, Shrink.empty)(a2 =>
        forall0(A3, Shrink.empty)(a3 =>
          forall0(A4, Shrink.empty)(a4 =>
            forall0(A5, Shrink.empty)(a5 =>
              prop(f(a1, a2, a3, a4, a5))
            )
          )
        )
      )
    )

  /** `forAll` with explicit `Gen` */
  def forAllG[A1](A1: Gen[A1])(f: A1 => Boolean): Property =
    forAll[A1](f)(A1)

  /** `forAll` with explicit `Gen` */
  def forAllG[A1, A2](A1: Gen[A1], A2: Gen[A2])(f: (A1, A2) => Boolean): Property =
    forAll[A1, A2](f)(A1, A2)

  /** `forAll` with explicit `Gen` */
  def forAllG[A1, A2, A3](A1: Gen[A1], A2: Gen[A2], A3: Gen[A3])(f: (A1, A2, A3) => Boolean): Property =
    forAll[A1, A2, A3](f)(A1, A2, A3)

  /** `forAll` with explicit `Gen` */
  def forAllG[A1, A2, A3, A4](A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4])(f: (A1, A2, A3, A4) => Boolean): Property =
    forAll[A1, A2, A3, A4](f)(A1, A2, A3, A4)

  /** `forAll` with explicit `Gen` */
  def forAllG[A1, A2, A3, A4, A5](A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5])(f: (A1, A2, A3, A4, A5) => Boolean): Property =
    forAll[A1, A2, A3, A4, A5](f)(A1, A2, A3, A4, A5)

  def property1[A1](f: A1 => Property)(implicit A1: Gen[A1], S1: Shrink[A1]): Property =
    forall0(A1, S1)(f)

  def property2[A1, A2](f: (A1, A2) => Property)(implicit A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1], S2: Shrink[A2]): Property =
    forall0(A1, S1)(a1 =>
      forall0(A2, S2)(a2 =>
        f(a1, a2)
      )
    )

  def property3[A1, A2, A3](f: (A1, A2, A3) => Property)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3]): Property =
    forall0(A1, S1)(a1 =>
      forall0(A2, S2)(a2 =>
        forall0(A3, S3)(a3 =>
          f(a1, a2, a3)
        )
      )
    )

  def property[A1](f: A1 => Property)(implicit A1: Gen[A1], S1: Shrink[A1]): Property =
    property1(f)

  def property[A1, A2](f: (A1, A2) => Property)(implicit A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1], S2: Shrink[A2]): Property =
    property2(f)

  def property[A1, A2, A3](f: (A1, A2, A3) => Property)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3]): Property =
    property3(f)

  object NoShrink {
    def property1[A1](f: A1 => Property)(implicit A1: Gen[A1], S1: Shrink[A1] = Shrink.empty[A1]): Property =
      Property.property1(f)

    def property2[A1, A2](f: (A1, A2) => Property)(implicit A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1] = Shrink.empty[A1], S2: Shrink[A2] = Shrink.empty[A2]): Property =
      Property.property2(f)

    def property3[A1, A2, A3](f: (A1, A2, A3) => Property)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1] = Shrink.empty[A1], S2: Shrink[A2] = Shrink.empty[A2], S3: Shrink[A3] = Shrink.empty[A3]): Property =
      Property.property3(f)
  }

}
