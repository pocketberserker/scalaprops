package scalaprops

import scala.util.Random
import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import scalaz.std.string._

object GenTest extends Scalaprops {

  private implicit def genGen[A](implicit A: Gen[A]): Gen[Gen[A]] = {
    val values = Gen[List[A]].sample(size = 100, seed = Random.nextLong())
    Gen.oneOf(
      Gen.value(A),
      List(
        values.map(
          x => Gen.value(Gen.value(x))
        ),
        List.fill(100)(Random.nextInt(Int.MaxValue - 1) + 1).map(x =>
          Gen.value(A.resize(x))
        ),
        List.fill(100)(Random.nextInt(Int.MaxValue - 1) + 1).map(x =>
          Gen.value(Gen.gen{ (i, r) =>
            val (r0, a) = A.f(i, r)
            (r0.reseed(x), a)
          })
        )
      ).flatten : _*
    )
  }

  implicit def genEqual[A](implicit A: Equal[A]): Equal[Gen[A]] =
    Equal.equal{ (x, y) =>
      Iterator.fill(100)((Random.nextInt(), Random.nextLong())).forall{
        case (size, seed) =>
          val r = Rand.standard(seed)
          Equal[(Rand, A)].equal(x.f(size, r), y.f(size, r))
      }
    }

  properties("Law") =
    Properties.list(
      scalazlaws.monad.all[Gen],
      scalazlaws.equal.all[Gen[Int]]
    )

  properties("test Gen.elements") = {
    val N = 5

    Property.forAllG(
      Gen.sequenceNList(1000, Gen[Rand]),
      Gen.sequenceNList(5, Gen[Int])
    ){ (rs, xs) =>
      val g = Gen.elements(xs.head, xs.tail: _*)
      val r = rs.map(r => g.f(Int.MaxValue, r)._2)
      (r.toSet == xs.toSet) && (xs.toSet.size == N)
    }.toCheckWith(Param.rand(Rand.fromSeed())).toProperties("test Gen.element")
  }

  property("test Gen.sequenceNList") = {
    val min = 5
    val max = 30
    val a = - 500
    val b = 20000

    Property.forAllG(
      Gen.choose(min, max).flatMap{ size =>
        Gen.sequenceNList(size, Gen.choose(a, b)).map(size -> _)
      }
    ){ case (size, values) =>
      (values.length == size) && (min <= size && size <= max) && values.forall{
        x => a <= x && x <= b
      }
    }
  }

  property("test Gen.frequencey") =
    Property.forAllG(
      Gen.sequenceNList(100, Gen.frequency(
        1 -> Gen.value(true),
        5 -> Gen.value(false)
      ))
    ){ list =>
      val (t, f) = list.partition(identity)
      (t.size < f.size) && t.nonEmpty
    }

  property("maybeGen") = Property.forAllG(
    Gen[Int], Gen.choose(100, 10000), Gen[Int]
  ){ (size, listSize, seed) =>
    val values = Gen[Maybe[Int]].samples(size = size, listSize = listSize, seed = seed)
    val just = values.count(_.isJust)
    (values.size == listSize) && (just > (listSize / 2)) && (just < listSize)
  }

  property("choose") = Property.forAll{ (a: Int, b: Int, size: Int, seed: Long) =>
    val x = Gen.choose(a, b).sample(size = size, seed = seed)
    val max = math.max(a, b)
    val min = math.min(a, b)
    (min <= x) && (x <= max)
  }

  property("listOfN 1") = Property.forAll{ (size0: Byte, seed: Long) =>
    val size = math.abs(size0.toInt)
    val result = Gen.listOfN(size, Gen[Unit]).map(_.size).sample(seed = seed)
    result <= size
  }

  property("listOfN 2") = Property.forAll{ seed: Long =>
    val size = 3
    Gen.listOfN(size, Gen[Unit]).map(_.size).samples(seed = seed, listSize = 100).distinct.size == (size + 1)
  }

  property("arrayOfN") = Property.forAll{ (size0: Byte, seed: Long) =>
    val size = math.abs(size0.toInt)
    val result = Gen.arrayOfN(size, Gen[Unit]).map(_.size).sample(seed = seed)
    result <= size
  }

  property("posLong") = Property.forAllG(Gen.positiveLong){_ > 0}
  property("posInt") = Property.forAllG(Gen.positiveInt){_ > 0}
  property("posShort") = Property.forAllG(Gen.positiveShort){_ > 0}
  property("posByte") = Property.forAllG(Gen.positiveByte){_ > 0}

  property("negLong") = Property.forAllG(Gen.negativeLong){_ < 0}
  property("negInt") = Property.forAllG(Gen.negativeInt){_ < 0}
  property("negShort") = Property.forAllG(Gen.negativeShort){_ < 0}
  property("negByte") = Property.forAllG(Gen.negativeByte){_ < 0}

  property("javaEnum") = Property.forAll{ seed: Int =>
    val values = Gen[java.util.concurrent.TimeUnit].samples(seed = seed, listSize = 200).toSet
    values == java.util.concurrent.TimeUnit.values().toSet
  }


  properties("genFunction") = {
    def test1[A: Gen: Cogen](name: String) =
      test[A, A](s"$name => $name")

    def test[A: Gen: Cogen, B: Gen](name: String) =
      Property.forAll { seed: Int =>
        val size = 10
        val as = Gen[A].infiniteStream(seed = seed).distinct.take(5).toList
        val values = Gen[A => B].samples(listSize = size, seed = seed).map(as.map(_))
        val set = values.toSet
        val result = set.size == size
        assert(result, s"$name ${set.size} $as $set $values")
        result
      }.toProperties(name)

    Properties.list(
      test1[Long]("Long"),
      test1[Int]("Int"),
      test1[Byte]("Byte"),
      test1[Short]("Short"),
      test1[Either[Byte, Short]]("Either[Byte, Short]"),
      test1[Long \/ Int]("""(Long \/ Int)"""),
      test1[(Int, Byte)]("Tuple2[Int, Byte]"),
      test1[Option[Int]]("Option[Int]"),
      test1[Map[Int, Int]]("Map[Int, Int]").andThenParam(Param.maxSize(10)),
      test1[Int ==>> Int]("(Int ==>> Int)").andThenParam(Param.maxSize(10)),
      test1[IList[Int]]("IList[Int]").andThenParam(Param.maxSize(10)),
      test1[Byte \&/ Byte]("""Byte \&/ Byte)"""),
      test[Int, List[Int]]("Int => List[Int]"),
      test[List[Int], Int]("List[Int] => Int").andThenParam(Param.maxSize(20)),
      test[Option[Byte], Byte]("Option[Byte] => Byte"),
      test[Byte, Option[Byte]]("Byte => Option[Byte]"),
      test[Either[Byte, Boolean], Int]("Either[Byte, Boolean] => Int"),
      test[Int, Map[Int, Boolean]]("Int => Map[Int, Boolean]")
    ).andThenParam(Param.minSuccessful(50))
  }

  properties("functionGenTest") = {

    def permutations[A](xs: IList[A], n: Int): IList[IList[A]] =
      if (xs.isEmpty) {
        IList.empty
      } else {
        def f(ls: IList[A], rest: IList[A], n: Int): IList[IList[A]] =
          if (n == 0) IList(ls) else rest.flatMap(v => f(v :: ls, rest, n - 1))
        f(IList.empty, xs, n)
      }

    def combinations[A: Order, B](as: IList[A], bs: IList[B]): IList[A ==>> B] = {
      val xs = permutations(bs, as.length)
      IList.fill(xs.length)(as).zip(xs).map {
        case (keys, values) =>
          keys.zip(values).toMap
      }
    }

    val defaultSize = 10000

    def test[A: Cogen : Order, B: Gen : Order](domain: IList[A], codomain: IList[B], name: String, streamSize: Int = defaultSize) = {
      import scalaz.std.stream._
      val size = List.fill(domain.length)(codomain.length).product

      Property.forAll { seed: Long =>
        val x = IList.fromFoldable(Gen[A => B].infiniteStream(seed = seed).map { f =>
          IMap.fromFoldable(domain.map(a => a -> f(a)))
        }.take(streamSize).distinct.take(size)).sorted

        assert(x.length == size, s"${x.length} != $size")
        Equal[IList[A ==>> B]].equal(x, combinations(domain, codomain).sorted)
      }.toProperties(name)
    }

    def test1[A: Cogen : Order : Gen](values: IList[A], name: String, streamSize: Int = defaultSize) =
      test[A, A](values, values, s"($name) => ($name)", streamSize)

    val orderingValues = IList[Ordering](Ordering.EQ, Ordering.GT, Ordering.LT)

    Properties.list(
      test1(IList(true, false), "Boolean"),
      test1(orderingValues, "Ordering"),
      test(IList(true, false), IList(Maybe.just(true), Maybe.just(false), Maybe.empty[Boolean]), "Boolean => Maybe[Boolean]"),
      test(IList(true, false), orderingValues, "Boolean => Ordering"),
      test(orderingValues, IList(true, false), "Ordering => Boolean"),
      test(IList(Maybe.just(true), Maybe.just(false), Maybe.empty[Boolean]), IList(true, false), "Maybe[Boolean] => Boolean"),
      test1(IList(Maybe.just(true), Maybe.just(false), Maybe.empty[Boolean]), "Maybe[Boolean]", 20000).andThenParam(Param.minSuccessful(20)),
      test1(IList(true, false).flatMap(a => IList(\/.right(a), \/.left(a))), """Boolean \/ Boolean""", 50000).andThenParam(Param.minSuccessful(5))
    )
  }

}
