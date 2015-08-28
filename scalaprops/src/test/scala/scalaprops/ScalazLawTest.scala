package scalaprops

object ScalazLawTest extends Scalaprops {
  properties("Laws") = scalazlaws.order.all[ScalazLaw]

  property("FullnameUnique") = Property.forAll{
    ScalazLaw.values.map(_.fullName).distinct.size == ScalazLaw.values.size
  }
}
