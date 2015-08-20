package scalaprops

abstract class Selector {
  def run[A](x: A, y: A): A
}

/**
 * [[https://github.com/nick8325/quickcheck/blob/2.8.1/Test/QuickCheck/Gen/Unsafe.hs#L36-L50]]
 */
object Selector {
  private[this] def genSelector[A]: Gen[(A, A) => A] =
    Gen.elements(
      (x, y) => x,
      (x, y) => y
    )

  implicit val genPolySelector: Gen[Selector] =
    Gen[Capture].map { c =>
      new Selector {
        def run[A](x: A, y: A) =
          c.capture(genSelector[A]).apply(x, y)
      }
    }
}
