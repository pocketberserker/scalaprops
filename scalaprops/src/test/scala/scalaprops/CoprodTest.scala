package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.option.optionEqual
import scalaz.std.list.listEqual

object CoprodTest extends Scalaprops{

  implicit val optionTriple: Triple[Option] =
    new Triple[Option] {
      override def etaInv[A](fa: Option[A]): Maybe[A] =
        Maybe.fromOption(fa)
      override def point[A](a: => A): Option[A] =
        Some(a)
      override def bind[A, B](fa: Option[A])(f: A => Option[B]) =
        fa flatMap f
      override def map[A, B](fa: Option[A])(f: A => B) =
        fa map f
    }

  implicit def eitherTriple[L]: Triple[({type l[a] = L \/ a})#l] =
    new Triple[({type l[a] = L \/ a})#l] {
      override def etaInv[A](fa: \/[L, A]) =
        fa.toMaybe
      override def point[A](a: => A) =
        \/-(a)
      override def map[A, B](fa: \/[L, A])(f: A => B) =
        fa map f
      override def bind[A, B](fa: \/[L, A])(f: A => \/[L, B]) =
        fa flatMap f
    }

  implicit val listTriple: Triple[List] =
    new Triple[List] {
      override def etaInv[A](fa: List[A]) =
        Maybe.fromOption(fa.headOption)

      override def point[A](a: => A) =
        a :: Nil

      override def bind[A, B](fa: List[A])(f: A => List[B]) =
        fa flatMap f

      override def map[A, B](fa: List[A])(f: A => B) =
        fa map f
    }


  private type CoprodOptionOption[A] = Coprod[Option, Option, A]
  private type DByte[a] = Byte \/ a
  private type CoprodOptionDisjunction[A] = Coprod[Option, DByte, A]
  private type CoprodOptionList[A] = Coprod[Option, List, A]

  val optionOption = scalazlaws.monad.all[CoprodOptionOption].andThenParam(Param.maxSize(3))
  val optionDisjunction = scalazlaws.monad.all[CoprodOptionDisjunction].andThenParam(Param.maxSize(3))

}
