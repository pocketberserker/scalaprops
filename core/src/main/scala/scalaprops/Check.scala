package scalaprops

import scalaz.{Applicative, Endo}

final case class CheckT[F[_]](prop: PropertyT[F], paramEndo: Endo[Param] = Param.id) {
  def toProperties[A](id: A): PropertiesT[F, A] =
    Properties.single(id, this)
  def ignore(reason: String)(implicit F: Applicative[F]): CheckT[F] =
    copy(prop = prop.ignore(reason))
}
