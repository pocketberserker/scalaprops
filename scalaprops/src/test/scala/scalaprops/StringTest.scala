package scalaprops

import scalaprops.Property.{forAll, forAllG}
import scalaz._
import scalaprops.GenTags._

object StringTest extends Scalaprops {

  private[this] def test[A](values: Seq[Char])(implicit
    M: Monoid[String @@ A],
    G: Gen[String @@ A],
    C: Cogen[String @@ A],
    S: Show[String @@ A],
    O: Order[String @@ A],
    I: IsEmpty[({type l[_] = String @@ A})#l]
  ) = {
    val expect = values.toSet
    val x = forAll{ str: (String @@ A) =>
      Tag.unwrap(str).forall(expect)
    }
    val y = Properties.list(
      scalazlaws.monoid.all[String @@ A],
      scalazlaws.order.all[String @@ A],
      scalazlaws.isEmpty.all[({type l[_] = String @@ A})#l]
    )
    x.toProperties(()).product(y)
  }

  properties("num") = test[GenTags.Num]('0' to '9')
  properties("upper") = test[GenTags.AlphaUpper]('A' to 'Z')
  properties("lower") = test[GenTags.AlphaLower]('a' to 'z')
  properties("alpha") = test[GenTags.Alpha](('a' to 'z') ++ ('A' to 'Z'))
  properties("alphaNum") = test[GenTags.AlphaNum](('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'))

  properties("genString") = {
    val g = for {
      min <- Gen.choose(-10, 50)
      s <- Gen.genString(Gen.asciiChar, min)
    } yield (min, s)

    val p = forAllG(g) {
      case (min, s) => s.length >= min
    }
    p.toProperties("minimum length")
  }

  property("nonEmptyString") = forAllG(Gen.nonEmptyString(Gen.asciiChar))(_.nonEmpty)

}
