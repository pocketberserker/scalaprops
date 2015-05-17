import scalaz.Id.Id

package object scalaprops {

  type Gen[A] = GenT[Id, A]
  val Gen = GenT

  type Property = PropertyT[Id]
  val Property = PropertyT

  type Properties[A] = PropertiesT[Id, A]
  val Properties = PropertiesT

  type Check = CheckT[Id]
  val Check = CheckT

}
