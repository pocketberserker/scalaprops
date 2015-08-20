package scalaprops

sealed abstract class Capture {
  def capture[A](gen: Gen[A]): A
}

object Capture {
  implicit val captureGen: Gen[Capture] =
    Gen.gen{ (n, r) =>
      val c = new Capture {
        def capture[A](gen: Gen[A]) =
          gen.f(n, r)._2
      }
      (r.next, c)
    }
}
