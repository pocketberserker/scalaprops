package scalaprops

import scalaz._

final case class PropertiesT[F[_], A] private (props: Tree[(A, Maybe[CheckT[F]])]) {
  def id: A = props.rootLabel._1

  private[this] def map[B](f: (A, Maybe[CheckT[F]]) => (B, Maybe[CheckT[F]])): PropertiesT[F, B] =
    Properties(props.map(f.tupled))

  def andThenParam(f: Endo[Param]): PropertiesT[F, A] =
    mapCheck(_.map(p => p.copy(paramEndo = p.paramEndo andThen f)))

  def andThenParamPF(f: PartialFunction[A, Endo[Param]]): PropertiesT[F, A] =
    Properties(props.map{
      case (i, Maybe.Just(m)) if f.isDefinedAt(i) =>
        i -> Maybe.just(CheckT(m.prop, m.paramEndo andThen f.apply(i)))
      case a => a
    })

  def composeParam(f: Endo[Param]): PropertiesT[F, A] =
    mapCheck(_.map(p => p.copy(paramEndo = p.paramEndo compose f)))

  def mapId[B](f: A => B): PropertiesT[F, B] =
    map((i, m) => (f(i), m))

  def mapRootId(f: A => A): PropertiesT[F, A] =
    Properties(Tree.node(f(props.rootLabel._1) -> props.rootLabel._2, props.subForest))

  def mapCheck(f: Maybe[CheckT[F]] => Maybe[CheckT[F]]): PropertiesT[F, A] =
    map((i, m) => (i, f(m)))

  def ignore(reason: String)(implicit F: Applicative[F]): PropertiesT[F, A] =
    mapCheck(_.map(_.ignore(reason)))

  def product[B](that: PropertiesT[F, B]): PropertiesT[F, Unit :-: A :-: B :-: Or.Empty] = {
    type T = Unit :-: A :-: B :-: Or.Empty
    Properties.noSort[F, T](
      Tree.node(
        Or[T](()) -> Maybe.empty[CheckT[F]],
        Stream(
          this.mapId(Or[T].apply(_)).props,
          that.mapId(Or[T].apply(_)).props
        )
      )
    )
  }
}

object PropertiesT {
  def either[A: Order, B: Order](id: A, prop0: Properties[B], props: Properties[B] *): Properties[A :-: B :-: Or.Empty] = {
    type T = A :-: B :-: Or.Empty
    fromProps[T](
      Or[T](id),
      prop0.mapId(Or[T].apply(_)),
      props.map(_.mapId(Or[T].apply(_))): _*
    )
  }

  def list[A: Order](prop0: Properties[A], props: Properties[A]*): Properties[Unit :-: A :-: Or.Empty] = {
    import scalaz.std.anyVal._
    either((), prop0, props: _*)
  }

  private[this] def ord1[A, B](implicit A: Order[A]): Order[(A, B)] =
    Order.orderBy(_._1)

  def single[F[_], A](id: A, c: CheckT[F]): PropertiesT[F, A] =
    PropertiesT[F, A](Tree.leaf(id -> Maybe.just(c)))

  def single[F[_], A](id: A, p: PropertyT[F]): PropertiesT[F, A] =
    PropertiesT[F, A](Tree.leaf(id -> Maybe.just(p.toCheck)))

  private[this] def properties0[A: Order](id: A, nodes: Stream[Tree[(A, Maybe[Check])]]): Properties[A] =
    Properties(distinctTree(Tree.node(
      id -> Maybe.empty[Check], nodes
    ))(ord1))

  private[scalaprops] def noSort[F[_], A](tree: Tree[(A, Maybe[CheckT[F]])]): PropertiesT[F, A] =
    Properties(tree)

  def properties[A: Order](id: A)(props: (A, Property) *): Properties[A] =
    properties0(
      id, props.map{case (n, p) => Tree.leaf(n -> Maybe.just(p.toCheck))}(collection.breakOut)
    )

  def fromChecks[A: Order](id: A)(checks: (A, Check) *): Properties[A] =
    properties0(
      id, checks.map{case (n, p) => Tree.leaf(n -> Maybe.just(p))}(collection.breakOut)
    )

  def fromProps[A: Order](id: A, prop0: Properties[A], props: Properties[A] *): Properties[A] =
    properties0(
      id, prop0.props #:: props.map(_.props).toStream
    )

  private[this] def distinctTree[A](tree: Tree[A])(implicit A: Order[A]): Tree[A] = {
    import std.stream._

    val x = Traverse[Tree].traverseS[ISet[A], A, Maybe[A]](tree){ a =>
      for{
        set <- State.get[ISet[A]]
        s <- if(set.contains(a)){
          State.state[ISet[A], Maybe[A]](Maybe.empty[A])
        }else {
          State((_: ISet[A]).insert(a) -> Maybe.just(a))
        }
      } yield s
    }.runZero._2

    def loop(t: Tree[Maybe[A]]): Maybe[Tree[A]] =
      t.rootLabel.map{ root =>
        Tree.node(root, MonadPlus[Stream].unite(t.subForest.map(loop)))
      }

    loop(x).toOption.get
  }
}
