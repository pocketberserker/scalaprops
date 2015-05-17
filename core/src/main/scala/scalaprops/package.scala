import scalaz.Id.Id

package object scalaprops {

  type Gen[A] = GenT[Id, A]

  val Gen = GenT

}
